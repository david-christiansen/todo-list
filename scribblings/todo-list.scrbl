#lang scribble/manual

@title{Todo List for DrRacket}
@author{David Thrane Christiansen}

@require[@for-label[racket todo-list syntax/srcloc]]
@declare-exporting[todo-list]

Todo List is a DrRacket tool that displays a list of the unwritten parts of a program, as determined
by the macros that implement those unwritten parts, as well as providing opportunities to write the
unfinished parts of the program with compiler support.

In particular, when Check Syntax finds a syntax object during expansion with the @racket['goal] syntax
property, then it considers that syntax object to be an unsolved goal. After expansion, a panel pops
up with a list of goals to be completed. If the syntax object additionally contains a
@racket['goal-summary] property, then the summary is used in the list and the full goal in the details
view. When it finds an object with the @racket['editing-command] property mapped to a description of
the editing command, then the editing commands are provided in a DrRacket right-click menu.

This tool is intended for use with cooperating languages, especially statically typed languages and
proof assistants. It is inspired by the hole list in the
@hyperlink["http://wiki.portal.chalmers.se/agda/pmwiki.php"]{Agda} mode for Emacs as well as the
ability of users to add custom interactive commands to Lean.

Open @tt{demo.rkt} from the package's source for a very simple hole macro and two editing commands.

The following syntax properties are recognized by the Todo List:
@itemlist[@item{@racket['goal]: the complete goal to be shown in the Todo List. A goal is either a
           string or an instance of the @racket[todo-item] prefab struct.}
          @item{@racket['editing-command]: an editing
           command to be shown in a region, which should be an instance of @racket[command].}]
To avoid a runtime dependency between your language and the Todo List, it is better to paste in the
source for the prefab structs @racket[todo-item] and @racket[command].

@defstruct*[todo-item ([location source-location?]
                       [full string?]
                       [summary (or/c #f string?)]) #:prefab]{
 A @racket[todo-item] represents a goal to be shown in the todo list. The goal is presented at
 @racket[loc], unless @racket[location] is @racket[#f], in which case the syntax object to which the
 goal is attached is used for its source location. The contents of @racket[full] are used as the
 contents of the goal details, and if @racket[summary] is @racket[#f], then @racket[full] is also
 used as the summary for the Todo List. If @racket[summary] is a string, then it is used as the
 summary. Only one goal is permitted for a region: if the @racket[location] fields cause multiple
 goals to overlap, then one will replace the other in an unspecified order.
}

@defstruct*[command ([name string?]
                     [module-path  	
                      (or/c module-path?
                            resolved-module-path?
                            module-path-index?)]
                     [function symbol?]
                     [arguments (listof any/c)])
            #:prefab]{
 An editing command. The @racket[name] is a string to be shown to users in a menu, the
 @racket[module-path] will be loaded with @racket[dynamic-require] to find the implementation of the
 command, and @racket[function] (which must be provided by the module) will be called with
 @racket[arguments]. If it returns a string, then the string is used to replace the region that was
 clicked on.

 Additionally, if @racket[function] accepts the following keyword arguments, then they will be
 provided as well: @racket[#:string] contains the string of the region on which the command was
 placed, @racket[#:definitions] is the DrRacket definitions window, @racket[#:editor] is the
 text editor object in which the command was called, and @racket[#:file] the path to the file being
 edited.

 While the values passed to @racket[#:definitions] and @racket[#:editor] presently coincide,
 if editing commands are ever supported in the interactions window, then they may not. So use
 @racket[#:editor] to get access to the widget in which the command is called, and
 @racket[#:definitions] to get access to the program context regardless of where the command
 is invoked.
}


