;;; init.el -*- lexical-binding: t; -*-

;; Copy this file to ~/.doom.d/init.el or ~/.config/doom/init.el ('doom
;; quickstart' will do this for you). The `doom!' block below controls what
;; modules are enabled and in what order they will be loaded. Remember to run
;; 'doom refresh' after modifying it.
;;
;; More information about these modules (and what flags they support) can be
;; found in modules/README.org.

;; Limit on depth in eval, apply and funcall before error
(setq max-lisp-eval-depth 10000)
;; Limit on number of Lisp variable bindings and unwind-protects.
(setq max-specpdl-size 10000)

(doom! :input
       ;; chinese
       ;; japanese

       :completion
       (company +childframe)           ; the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       (ivy +icons)               ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       hydra
       ;; indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;; ligatures         ; ligatures and symbols to make your code pretty again
       ;; tabs
       treemacs          ; a project drawer, like neotree but cooler
       ;; unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       format  ; automated prettiness
       ;;lispy             ; vim for lisp, for people who dont like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       (parinfer +rust)          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;; word-wrap

       :emacs
       (dired +ranger +icons)
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)
       vc                ; version-control and Emacs, sitting in a tree
       undo

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       ;; term              ; terminals in Emacs
       vterm             ; another terminals in Emacs

       :checkers
       (syntax)              ; tasing you for every semicolon you forget
       ;; spell             ; tasing you for misspelling mispelling
       ;; grammar           ; tasing grammar mistake every you make

       :tools
       ;; ansible
       ;; debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       docker
       ;; editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       gist              ; interacting with github gists
       (lookup +dictionary +docsets)
       (lsp +peek)
       (magit +forge)             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       (pass +auth)              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       (cc +lsp)               ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;erlang            ; an elegant language for a more civilized age
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       ;;fsharp           ; ML stands for Microsoft's Language
       (go +lsp)               ; the hipster dialect
       ;;(haskell        ; a language that's lazier than I am
       ;;+intero)
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;; (java +meghanada)
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       latex             ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml           ; an objective camel
       (org              ; organize your plain life in plain text
        +brain
        +dragndrop       ; file drag & drop support
        +gnuplot
        +hugo
        +journal
        +jupyter
        +pandoc          ; pandoc integration into org's exporter
        +pomodoro
        +roam
        +present)        ; using Emacs for presentations
       ;; perl              ; write code no one else can comprehend
       (php +lsp)                ; perl's insecure younger brother
       plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +pyenv)
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;; rest              ; Emacs as a REST client
       ;;ruby            ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       (sh +lsp)                ; she sells {ba,z,fi}sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web +lsp)               ; the tubes
       ;;vala              ; GObjective-C

       :email
       ;; (:if IS-MAC (mu4e +gmail))       ; WIP
       ;; notmuch             ; WIP
       ;;(wanderlust +gmail) ; WIP

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       calendar
       ;; irc              ; how neckbeards socialize
       (rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :collab
       ;;floobits          ; peer programming for a price
       ;;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +bindings +smartparens)

       :cnsunyour
       bindings
       beancount
       blog
       calendar
       chinese
       ;; ebook
       editor
       ;; (:if IS-MAC email)
       ;; irc
       ;; weechat
       org
       ;; tabnine
       telega
       term
       tools
       translate
       ui
       love)

(setq custom-file (expand-file-name ".custom.el" doom-private-dir))
(load custom-file t)
