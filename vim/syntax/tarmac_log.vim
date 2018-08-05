" Gabriel's tarmac syntax file

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match

"syn match   tcInt     /-\?\<\d\+\>/
syn region tcMismatch start="^.*MISMATCH" end=">>>> RTL" contains=tcFirstLine
syn match   tcInt     /\<\x\+\>/ contained 
syn match tcCcfail "CCFAIL" contained 
syn match tcDisass ":[^:]*$"ms=s+1 contains=tcCcfail containedin=tcFirstLine
syn match tcContext ".......:"me=e-1  containedin=tcFirstLine
syn region tcOpcode start="(" end=")"  containedin=tcFirstLine
syn match tcType "....("me=e-1 containedin=tcFirstLine 
syn match  tcTime   /\d\+ \(ns\|ps\|tic\)/ containedin=tcFirstLine
syn region tcRegister start="^ \+\(R\|LD\|ST\)" end="$" contains=tcInt
syn match tcFirstLine "\d\+.*(.*).*:.*$" contains=tcTime,tcInt,tcOpcode,tcType,tcContext,tcCcfail
syn match tcEXC ".*EXC .*"
syn match tcHeader "^Tarmac.*"
syn match ctLine "ISS::.*" contains=tcOx
syn keyword tcOx 0x containedin=ctLine
if version >= 508 || !exists("did_proto_syn_inits")
  if version < 508
    let did_proto_syn_inits = 1
    command! -nargs=+ HiLink hi link <args>
  else
    command! -nargs=+ HiLink hi def link <args>
  endif


  HiLink tcInt Number
  HiLink tcTime Type
  HiLink tcType Special
  HiLink tcDisass String
  HiLink tcOpcode Include
  HiLink tcCcfail Keyword
"  HiLink tcFirstLine Error
  HiLink tcRegister Delimiter
  HiLink tcContext Delimiter
"  HiLink tcEXC Error
  HiLink tcHeader Comment
  HiLink tcOx Comment
  HiLink ctLine Identifier
  HiLink tcMismatch Error
  delcommand HiLink
endif

