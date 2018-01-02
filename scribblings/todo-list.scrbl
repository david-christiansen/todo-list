#lang scribble/manual

@title{Todo List for DrRacket}
@author{David Thrane Christiansen}

@require[@for-label[racket todo-list syntax/srcloc]]
@require[racket/runtime-path]
@declare-exporting[todo-list]

Todo List is a DrRacket tool that displays a list of the unwritten parts of a program, as determined
by the macros that implement those unwritten parts, as well as providing opportunities to write the
unfinished parts of the program with compiler support.

@define-runtime-path[demo.png "../demo.png"]
@image[demo.png #:scale 0.45]{Screenshot of the Todo List}


In particular, when Check Syntax finds a syntax object during expansion with the @racket['todo] syntax
property, then it considers that syntax object to be a task to be completed. After expansion, a panel pops
up with a list of these tasks to be completed. When it finds an object with the @racket['editing-command]
property mapped to a description of the editing command, then the editing commands are provided in a
DrRacket right-click menu.

This tool is intended for use with cooperating languages, especially statically typed languages and
proof assistants. It is inspired by the hole list in the
@hyperlink["http://wiki.portal.chalmers.se/agda/pmwiki.php"]{Agda} mode for Emacs as well as the
ability of users to add custom interactive commands to Lean.

Open @tt{demo.rkt} from the package's source for a very simple hole macro and two editing commands.

The following syntax properties are recognized by the Todo List:
@itemlist[@item{@racket['todo]: the complete task to be shown in the Todo List. A TODO is either a
           string, an instance of the @racket[todo-item] prefab struct, or an instance of
           @racket[located] with a @racket[todo-item] inside.}
          @item{@racket['editing-command]: an editing
           command to be shown in a region. A command is a cons tree of instances of @racket[command]
           or instances of @racket[located] with a @racket[command] inside.}]
To avoid a runtime dependency between your language and the Todo List, it is better to paste in the
source for the prefab structs @racket[todo-item], @racket[command], and @racket[located] than to
require them directly.

@defstruct*[todo-item ([full string?]
                       [summary (or/c #f string?)]) #:prefab]{
 A @racket[todo-item] represents an item to be shown in the todo list.
 The contents of @racket[full] are used as the contents of the details pane, and if @racket[summary]
 is @racket[#f], then @racket[full] is also used as the summary for the Todo List. If @racket[summary]
 is a string, then it is used as the summary.
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

@defstruct*[located ([location source-location?]
                     [value any/c])
            #:prefab]{
 A an item in the Todo List or a command can be put inside a @racket[located] struct. The
 @racket[location] field contains the region to associate it with, and the @racket[value]
 field contains the TODO or command.

 Only one TODO is permitted for a region: if the @racket[location] fields cause multiple
 goals to overlap, then one will replace the other in an unspecified order.
}

