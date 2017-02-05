" Copyright (c) 2016 Junegunn Choi
" Copyright (c) 2017 Andrey Popp

if exists('g:loaded_fzy')
  finish
endif
let g:loaded_fzy = 1

let s:cpo_save = &cpo
set cpo&vim

""""" Core

let s:default_layout = { 'down': '~40%' }
let s:layout_keys = ['window', 'up', 'down', 'left', 'right']
let s:fzy_executable = 'fzy'

function! s:fzy_exec()
  if exists('g:fzy_executable')
    let s:exec = g:fzy_executable
  elseif !exists('s:exec')
    if executable(s:fzy_executable)
      let s:exec = s:fzy_executable
    elseif executable('fzy')
      let s:exec = 'fzy'
    else
      redraw
      throw 'fzy executable not found'
    endif
  endif
  return s:exec
endfunction

function! s:shellesc(arg)
  return '"'.substitute(a:arg, '"', '\\"', 'g').'"'
endfunction

function! s:escape(path)
  return escape(a:path, ' $%#''"\')
endfunction

function! s:error(msg)
  echohl ErrorMsg
  echom a:msg
  echohl None
endfunction

function! s:warn(msg)
  echohl WarningMsg
  echom a:msg
  echohl None
endfunction

function! s:has_any(dict, keys)
  for key in a:keys
    if has_key(a:dict, key)
      return 1
    endif
  endfor
  return 0
endfunction

function! s:open(cmd, target)
  if stridx('edit', a:cmd) == 0 && fnamemodify(a:target, ':p') ==# expand('%:p')
    return
  endif
  execute a:cmd s:escape(a:target)
endfunction

function! s:common_sink(lines) abort
  if len(a:lines) < 1
    return
  endif
  let cmd = 'e'
  if len(a:lines) > 1
    augroup fzy_swap
      autocmd SwapExists * let v:swapchoice='o'
            \| call s:warn('fzy: E325: swap file exists: '.expand('<afile>'))
    augroup END
  endif
  try
    let empty = empty(expand('%')) && line('$') == 1 && empty(getline(1)) && !&modified
    let autochdir = &autochdir
    set noautochdir
    for item in a:lines
      if empty
        execute 'e' s:escape(item)
        let empty = 0
      else
        call s:open(cmd, item)
      endif
      if exists('#BufEnter') && isdirectory(item)
        doautocmd BufEnter
      endif
    endfor
  finally
    let &autochdir = autochdir
    silent! autocmd! fzy_swap
  endtry
endfunction

" [name string,] [opts dict,] [fullscreen boolean]
function! fzy#wrap(...)
  let args = ['', {}, 0]
  let expects = map(copy(args), 'type(v:val)')
  let tidx = 0
  for arg in copy(a:000)
    let tidx = index(expects, type(arg), tidx)
    if tidx < 0
      throw 'invalid arguments (expected: [name string] [opts dict] [fullscreen boolean])'
    endif
    let args[tidx] = arg
    let tidx += 1
    unlet arg
  endfor
  let [name, opts, bang] = args

  " Layout: g:fzy_layout
  if bang
    for key in s:layout_keys
      if has_key(opts, key)
        call remove(opts, key)
      endif
    endfor
    let opts = extend(opts, {'window': 'enew'})
  elseif !s:has_any(opts, s:layout_keys)
    let opts = extend(opts, get(g:, 'fzy_layout', s:default_layout))
  endif

  let opts.options = get(opts, 'options', '')

  " Action: g:fzy_action
  if !s:has_any(opts, ['sink', 'sink*'])
    function! opts.sink(lines) abort
      return s:common_sink(a:lines)
    endfunction
    let opts['sink*'] = remove(opts, 'sink')
  endif

  return opts
endfunction

function! fzy#run(...) abort
try
  let oshell = &shell
  set shell=sh
  if has('nvim') && len(filter(range(1, bufnr('$')), 'bufname(v:val) =~# ";#FZY"'))
    call s:warn('FZY is already running!')
    return []
  endif
  let dict   = exists('a:1') ? a:1 : {}
  let temps  = { 'result': tempname() }
  let optstr = get(dict, 'options', '')
  try
    let fzy_exec = s:fzy_exec()
  catch
    throw v:exception
  endtry

  if !has_key(dict, 'source') && !empty($FZY_DEFAULT_COMMAND)
    let temps.source = tempname()
    call writefile(split($FZY_DEFAULT_COMMAND, "\n"), temps.source)
    let dict.source = (empty($SHELL) ? 'sh' : $SHELL) . ' ' . s:shellesc(temps.source)
  endif

  if has_key(dict, 'source')
    let source = dict.source
    let type = type(source)
    if type == 1
      let prefix = source.'|'
    elseif type == 3
      let temps.input = tempname()
      call writefile(source, temps.input)
      let prefix = 'cat '.s:shellesc(temps.input).'|'
    else
      throw 'invalid source type'
    endif
  else
    let prefix = ''
  endif

  let command = prefix.fzy_exec.' '.optstr

  if has('nvim')
    return s:execute_term(dict, command, temps.result, temps)
  endif

  let lines = s:execute(dict, command, temps.result, temps)
  call s:callback(dict, lines)
  return lines
finally
  let &shell = oshell
endtry
endfunction

function! s:present(dict, ...)
  for key in a:000
    if !empty(get(a:dict, key, ''))
      return 1
    endif
  endfor
  return 0
endfunction

function! s:splittable(dict)
  return s:present(a:dict, 'up', 'down') && &lines > 15 ||
        \ s:present(a:dict, 'left', 'right') && &columns > 40
endfunction

function! s:pushd(dict)
  if s:present(a:dict, 'dir')
    let cwd = getcwd()
    if get(a:dict, 'prev_dir', '') ==# cwd
      return 1
    endif
    let a:dict.prev_dir = cwd
    execute 'lcd' s:escape(a:dict.dir)
    let a:dict.dir = getcwd()
    return 1
  endif
  return 0
endfunction

augroup fzy_popd
  autocmd!
  autocmd WinEnter * call s:dopopd()
augroup END

function! s:dopopd()
  if !exists('w:fzy_prev_dir') || exists('*haslocaldir') && !haslocaldir()
    return
  endif
  execute 'lcd' s:escape(w:fzy_prev_dir)
  unlet w:fzy_prev_dir
endfunction

function! s:exit_handler(code, command, ...)
  if a:code == 130
    return 0
  elseif a:code > 1
    call s:error('Error running ' . a:command)
    if !empty(a:000)
      sleep
    endif
    return 0
  endif
  return 1
endfunction

function! s:execute(dict, command, out, temps) abort
  call s:pushd(a:dict)
  silent! !clear 2> /dev/null
  let command = escape(substitute(a:command'.' > '.a:out, '\n', '\\n', 'g'), '%#')
  execute 'silent !'.command
  let exit_status = v:shell_error
  redraw!
  return s:exit_handler(exit_status, command) ? s:collect(a:temps) : []
endfunction

function! s:calc_size(max, val, dict)
  let val = substitute(a:val, '^\~', '', '')
  if val =~ '%$'
    let size = a:max * str2nr(val[:-2]) / 100
  else
    let size = min([a:max, str2nr(val)])
  endif

  let srcsz = -1
  if type(get(a:dict, 'source', 0)) == type([])
    let srcsz = len(a:dict.source)
  endif

  let margin = 1
  return srcsz >= 0 ? min([srcsz + margin, size]) : size
endfunction

function! s:getpos()
  return {'tab': tabpagenr(), 'win': winnr(), 'cnt': winnr('$'), 'tcnt': tabpagenr('$')}
endfunction

function! s:split(dict, command)
  let next_command = a:command
  let directions = {
  \ 'up':    ['topleft', 'resize', &lines],
  \ 'down':  ['botright', 'resize', &lines],
  \ 'left':  ['vertical topleft', 'vertical resize', &columns],
  \ 'right': ['vertical botright', 'vertical resize', &columns] }
  let ppos = s:getpos()
  try
    if s:present(a:dict, 'window')
      execute a:dict.window
      let next_command = next_command.' --lines '.&lines
    elseif !s:splittable(a:dict)
      execute (tabpagenr()-1).'tabnew'
      let next_command = next_command.' --lines '.&lines
    else
      for [dir, triple] in items(directions)
        let val = get(a:dict, dir, '')
        if !empty(val)
          let [cmd, resz, max] = triple
          if (dir == 'up' || dir == 'down') && val[0] == '~'
            let sz = s:calc_size(max, val, a:dict)
          else
            let sz = s:calc_size(max, val, {})
          endif
          let next_command = next_command.' --lines '.sz
          execute cmd sz.'new'
          execute resz sz
          return [ppos, {}, next_command]
        endif
      endfor
    endif
    return [ppos, { '&l:wfw': &l:wfw, '&l:wfh': &l:wfh }, next_command]
  finally
    setlocal winfixwidth winfixheight
  endtry
endfunction

function! s:execute_term(dict, command, out, temps) abort
  let winrest = winrestcmd()
  let [ppos, winopts, cmd] = s:split(a:dict, a:command)
  let fzy = { 'buf': bufnr('%'), 'ppos': ppos, 'dict': a:dict, 'temps': a:temps,
            \ 'winopts': winopts, 'winrest': winrest, 'lines': &lines,
            \ 'columns': &columns, 'command': a:command }
  function! fzy.switch_back(inplace)
    if a:inplace && bufnr('') == self.buf
      " FIXME: Can't re-enter normal mode from terminal mode
      " execute "normal! \<c-^>"
      b #
      " No other listed buffer
      if bufnr('') == self.buf
        enew
      endif
    endif
  endfunction
  function! fzy.on_exit(id, code)
    if s:getpos() == self.ppos " {'window': 'enew'}
      for [opt, val] in items(self.winopts)
        execute 'let' opt '=' val
      endfor
      call self.switch_back(1)
    else
      if bufnr('') == self.buf
        " We use close instead of bd! since Vim does not close the split when
        " there's no other listed buffer (nvim +'set nobuflisted')
        close
      endif
      execute 'tabnext' self.ppos.tab
      execute self.ppos.win.'wincmd w'
    endif

    if bufexists(self.buf)
      execute 'bd!' self.buf
    endif

    if &lines == self.lines && &columns == self.columns && s:getpos() == self.ppos
      execute self.winrest
    endif

    if !s:exit_handler(a:code, self.command, 1)
      return
    endif

    call s:pushd(self.dict)
    let lines = s:collect(self.temps)
    call s:callback(self.dict, lines)
    call self.switch_back(s:getpos() == self.ppos)
  endfunction

  try
    if s:present(a:dict, 'dir')
      execute 'lcd' s:escape(a:dict.dir)
    endif
    call termopen(cmd.' > '.a:out.';#FZY', fzy)
  finally
    if s:present(a:dict, 'dir')
      lcd -
    endif
  endtry
  setlocal nospell bufhidden=wipe nobuflisted
  setf fzy
  startinsert
  return []
endfunction

function! s:collect(temps) abort
  try
    return filereadable(a:temps.result) ? readfile(a:temps.result) : []
  finally
    for tf in values(a:temps)
      silent! call delete(tf)
    endfor
  endtry
endfunction

function! s:callback(dict, lines) abort
  " Since anything can be done in the sink function, there is no telling that
  " the change of the working directory was made by &autochdir setting.
  "
  " We use the following heuristic to determine whether to restore CWD:
  " - Always restore the current directory when &autochdir is disabled.
  "   FIXME This makes it impossible to change directory from inside the sink
  "   function when &autochdir is not used.
  " - In case of an error or an interrupt, a:lines will be empty.
  "   And it will be an array of a single empty string when fzy was finished
  "   without a match. In these cases, we presume that the change of the
  "   directory is not expected and should be undone.
  let popd = has_key(a:dict, 'prev_dir') &&
        \ (!&autochdir || (empty(a:lines) || len(a:lines) == 1 && empty(a:lines[0])))
  if popd
    let w:fzy_prev_dir = a:dict.prev_dir
  endif

  try
    if has_key(a:dict, 'sink')
      for line in a:lines
        if type(a:dict.sink) == 2
          call a:dict.sink(line)
        else
          execute a:dict.sink s:escape(line)
        endif
      endfor
    endif
    if has_key(a:dict, 'sink*')
      call a:dict['sink*'](a:lines)
    endif
  catch
    if stridx(v:exception, ':E325:') < 0
      echoerr v:exception
    endif
  endtry

  " We may have opened a new window or tab
  if popd
    let w:fzy_prev_dir = a:dict.prev_dir
    call s:dopopd()
  endif
endfunction

""""" Files

function! fzy#files(bang, ...) abort
  let args = copy(a:000)
  let opts = { 'options': '' }
  if len(args) && isdirectory(expand(args[-1]))
    let dir = substitute(remove(args, -1), '/*$', '/', '')
    let dir_title = substitute(dir, '\\\(["'']\)', '\1', 'g')
    let opts.options .= ' --prompt '.shellescape(dir_title)
    let opts.dir = dir
  else
    let dir = getcwd()
    let dir_title = pathshorten(dir).'/'
    let opts.options .= ' --prompt '.shellescape(dir_title)
    let opts.dir = dir
  endif
  let opts.options .= ' '.join(args)
  call fzy#run(fzy#wrap('FZY', opts, a:bang))
endfunction

""""" Buffers

let g:fzy#buffers = {}
augroup fzy_buffers
  autocmd!
  if exists('*reltimefloat')
    autocmd BufWinEnter,WinEnter * let g:fzy#buffers[bufnr('')] = reltimefloat(reltime())
  else
    autocmd BufWinEnter,WinEnter * let g:fzy#buffers[bufnr('')] = localtime()
  endif
  autocmd BufDelete * silent! call remove(g:fzy#buffers, expand('<abuf>'))
augroup END

function! s:buflisted()
  return filter(range(1, bufnr('$')), 'buflisted(v:val) && getbufvar(v:val, "&filetype") != "qf" && v:val != bufnr("")')
endfunction

function! s:strip(str)
  return substitute(a:str, '^\s*\|\s*$', '', 'g')
endfunction

function! s:string_pad_right(s, amt, ...)
  if a:0 > 0
    let char = a:1
  else
    let char = ' '
  endif
  return a:s . repeat(char, a:amt - len(a:s))
endfunction

function! s:string_pad_left(s, amt, ...)
  if a:0 > 0
    let char = a:1
  else
    let char = ' '
  endif
  return repeat(char,a:amt - len(a:s)) . a:s
endfunction

function! s:bufopen(lines)
  if len(a:lines) < 1
    return
  endif
  let b = matchstr(a:lines[0], '\[\zs[0-9]*\ze\]')
  execute 'buffer' b
endfunction

function! s:format_buffer(b)
  let bufnr_pad = len(string(bufnr('$')))
  let name = bufname(a:b)
  let name = empty(name) ? '[no name]' : fnamemodify(name, ":~:.")
  let flag = a:b == bufnr('')  ? '%' :
          \ (a:b == bufnr('#') ? '#' : ' ')
  let modified = getbufvar(a:b, '&modified') ? ' [modified]' : ''
  let readonly = getbufvar(a:b, '&modifiable') ? '' : ' [readonly]'
  let extra = join(filter([modified, readonly], '!empty(v:val)'), '')
  return s:strip(printf("[%s] %s  %s  %s", s:string_pad_left(a:b, bufnr_pad), flag, name, extra))
endfunction

function! s:sort_buffers(...)
  let [b1, b2] = map(copy(a:000), 'get(g:fzy#buffers, v:val, v:val)')
  " Using minus between a float and a number in a sort function causes an error
  return b1 > b2 ? 1 : -1
endfunction

function! fzy#buffers(bang, ...)
  let bufs = map(sort(s:buflisted(), 's:sort_buffers'), 's:format_buffer(v:val)')
  let query = a:1
  let opts = {
  \ 'source':  reverse(bufs),
  \ 'sink*':   function('s:bufopen'),
  \ 'options': '--prompt="buffer> "'
  \}
  if !empty(query)
    let opts.options .= ' --query='.shellescape(query)
  endif
  call fzy#run(fzy#wrap('FZY', opts, a:bang))
endfunction

""""" Commands

command! -nargs=* -complete=dir -bang FZYFiles call fzy#files(<bang>0, <f-args>)
command! -bar -bang -nargs=? -complete=buffer FZYBuffers call fzy#buffers(<bang>0, <q-args>)

let &cpo = s:cpo_save
unlet s:cpo_save
