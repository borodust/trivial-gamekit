(cl:in-package :trivial-gamekit.documentation)


(docstring (macro-function 'defgame)
  "Defines a game class that can be passed to [`#'start`](#gamekit-start) to run
a game. `name` is the name of a class generated. `classes` are names of
superclasses, `slots` - standard class slots and `opts` are class options. So,
pretty much standard class definition except it does configure a class in
certain ways specifically for `gamekit` use and allows passing additional
options in `opts` apart from standard `:documentation`, `:default-initargs` and
so others.

Additional options that can be passed in `opts` are:

* `:viewport-width` - width of the window and canvas
* `:viewport-height` - height of the window and canvas
* `:viewport-title` - title of the window
* `:prepare-resources` - boolean value that indicates whether `gamekit` should
load resources automatically on startup or if not, user prefers to load them dynamically on request. Defaults to `t`.

Example:

```common-lisp
\(gamekit:defgame example ()
  ;; some game related state
  ((world :initform (make-instance 'world))
   (game-state))
  ;; options
  (:viewport-width 800)
  (:viewport-height 600)
  (:viewport-title \"EXAMPLE\")
  (:prepare-resources nil))
```")


(docstring #'gamekit
  "Returns instance of a running game or `nil` if no game is started yet.

Example:
```common-lisp
 (gamekit:gamekit)
```")


(docstring #'act
  "Called every game loop iteration for user to add any per-frame behavior to
the game. NOTE: all drawing operations should be performed in
[`#'draw`](#gamekit-draw) method.

Example:
```common-lisp
 (defmethod gamekit:act ((this example))
   (report-fps))
```")


(docstring #'draw
  "Called every game loop iteration for frame rendering.
All drawing operations should be performed in this method.

Example:
```common-lisp
 (defmethod gamekit:draw ((this example))
   (gamekit:draw-text \"Hello, Gamedev!\" (gamekit:vec2 10 10)))
```")


(docstring #'post-initialize
  "This function is called after game instance is fully initialized, right
before main game loop starts its execution. Put initialization code for your
application into method of this function. For example, it would be logical to
bind input via [`#'bind-cursor`](#gamekit-bind-cursor) or
[`#'bind-button`](#gamekit-bind-button) here.

Example:
```common-lisp
\(defmethod gamekit:post-initialize ((this example))
  (init-game)
  (bind-input))
```")


(docstring #'pre-destroy
  "This function is called just before shutting down a game instance for you to
free all acquired resources and do any other clean up procedures.

Example:
```common-lisp
 (defmethod gamekit:pre-destroy ((this example))
   (release-foreign-memory)
   (stop-threads))
```")


(docstring #'notice-resources
  "Called when resource names earlier requested with
[`#'prepare-resources`](#gamekit-prepare-resources) which indicates those
resources are ready to be used.

Override this generic function to know when resources are ready.

Example:
```common-lisp
 (defmethod gamekit:notice-resources ((this example) &rest resource-names)
   (declare (ignore resource-names))
   (gamekit:play-sound 'example-package::blop)
   (show-start-screen))
```")


(docstring #'prepare-resources
  "Loads and prepares resources for later usage asynchronously. `resource-names`
should be symbols used previously registered with `define-*` macros.

This function returns immediately. When resources are ready for use
[`#'notice-resources`](#gamekit-notice-resources) will be called with names that
were passed to this function.

`gamekit` by default will try to load and prepare all registered resources on
startup which might take a substantial time, but then you don't need to call
#'prepare-resources yourself. If you prefer load resources on demand and have a
faster startup time, pass nil to :prepare-resources option of a
[`defgame`](#gamekit-defgame) macro which will disable startup resource
autoloading.

Example:
```common-lisp
 (gamekit:prepare-resources 'example-package::noto-sans
                            'example-package::blop
                            'example-package::logo)
```")


(docstring #'dispose-resources
  "Disposes prepared resources asynchronously. `resource-names`
should be symbols used previously registered with `define-*` macros.

This function returns immediately. Attempts to use disposed resources will raise
an error. To use resources again you would need to load them with
[`#'prepare-resources`](#gamekit-prepare-resources).

Example:
```common-lisp
 (gamekit:dispose-resources 'example-package::noto-sans
                            'example-package::blop
                            'example-package::logo)
```")


(docstring #'bind-button
  "Binds `action` to specified `key` `state`. When key state changes to the one specified,
action callback is invoked with no arguments. `#'bind-button` function should be
called when there's active game exists started earlier with
[`#'start`](#gamekit-start). `state` can be either `:pressed`, `:released` or
`:repeating`.

Actions are not stacked together and would be overwritten for the same key and state.

Can only be called when gamekit instance is active (started).

Supported keys:
```common-lisp
  :space :apostrophe :comma :minus :period :slash
  :0 :1 :2 :3 :4 :5 :6 :7 :8 :9
  :semicolon :equal
  :a :b :c :d :e :f :g :h :i :j :k :l :m
  :n :o :p :q :r :s :t :u :v :w :x :y :z
  :left-bracket :backslash :right-bracket
  :grave-accent :world-1 :world-2
  :escape :enter :tab :backspace :insert :delete
  :right :left :down :up
  :page-up :page-down :home :end
  :caps-lock :scroll-lock :num-lock :print-screen :pause
  :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12
  :f13 :f14 :f15 :f16 :f17 :f18 :f19 :f20 :f21 :f22 :f23 :f24 :f25
  :keypad-0 :keypad-1 :keypad-2 :keypad-3 :keypad-4
  :keypad-5 :keypad-6 :keypad-7 :keypad-8 :keypad-9
  :keypad-decimal :keypad-divide :keypad-multiply
  :keypad-subtract :keypad-add :keypad-enter :keypad-equal
  :left-shift :left-control :left-alt :left-super
  :right-shift :right-control :right-alt :right-super
  :menu

  :mouse-left :mouse-right :mouse-middle
```

Example
```common-lisp
\(gamekit:bind-button :enter :pressed
                     (lambda ()
                       (start-game-for *player*)))
```")


(docstring #'bind-any-button
  "Binds `action` to all buttons. When key state changes, action callback is
invoked with two arguments: button as a first and its new state as a second
argument.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

For possible values for button and state parameters see documentation for
 [`#'bind-button`](#gamekit-bind-button) function.

Actions provided to this function are not stacked together and would be
overwritten each time the function is called.


Example
```common-lisp
 (gamekit:bind-any-button (lambda (button state)
                            (when (and (eq button :space) (eq state :pressed))
                              (shoot *player*))))
```")


(docstring #'bind-cursor
  "Binds action callback to a cursor movement event. Everytime user moves a
cursor callback will be called with x and y of cursor coordinates within the
same coordinate system canvas is defined in: bottom left corner as (0,0) origin
and y-axis pointing upwards.

Actions doesn't stack together and would be overwritten each time
`#'bind-cursor` is called.

Can only be called when gamekit instance is active (started).

Example:
```common-lisp
 (gamekit:bind-cursor (lambda (x y)
                        (shoot-to x y)))
```")


(docstring #'play-sound
  "Plays a sound defined earlier with [`define-sound`](#gamekit-define-sound). Pass `t` to
`:looped-p` key to play sound in a loop.

Example:
```common-lisp
 (gamekit:play-sound 'example-package::blop
                     :looped-p t)
```")


(docstring #'stop-sound
  "Stops a playing sound by provided sound id.

Example:
```common-lisp
 (gamekit:stop-sound 'example-package::blop)
```")


(docstring #'play
  "Deprecated. Use #'play-sound instead")


(docstring #'make-font
  "Makes a font instance that can be later passed to [`#'draw-text`](#gamekit-draw-text) to
customize text looks. `font-id` must be a valid resource name previously registered with
[`define-font`](#gamekit-define-font). Second argument is a font size in pixels.

Example:
```common-lisp
 (gamekit:make-font 'example-package::noto-sans 32)
```")


(docstring #'calc-text-bounds
  "Calculates text bounds with the font provided or the default one otherwise and returns
several values: origin as vec2, width, height and calculated advance

Example:
```common-lisp
\(gamekit:calc-text-bounds \"hello there\"\)
```")


(docstring #'print-text
  "Deprecated. Use #'draw-text instead")


(docstring #'start
  "Bootsraps a game allocating a window and other system resources. Instantiates
game object defined with [`defgame`](#gamekit-defgame) which can be obtained via
[`#'gamekit`](#gamekit-gamekit). Cannot be called twice -
[`#'stop`](#gamekit-stop) should be called first before running `start` again.

Example:

```common-lisp
\(gamekit:start 'example\)
```")


(docstring #'stop
  "Stops currently running game releasing acquired resources.

Example:
```common-lisp
\(gamekit:stop\)
```")


(docstring #'register-resource-package
  "Associates resource package with filesystem path. For proper resource
handling it is recommended to put it as a top-level form, so resources could be
located at load-time.

First argument, a package name, must be a valid Common Lisp package name that
could be used to locate package via #'find-package. Second argument is a
filesystem path to a directory where resources can be found.

Example:
```common-lisp
 (gamekit:register-resource-package :example-package
                                    \"/home/gamdev/example-game/assets/\")
```")


(docstring (macro-function 'define-image)
  "Registers image resource by name that can be used by
[`#'draw-image`](#gamekit-draw-image) later. Second argument is a valid path to
the resource.  Only .png images are supported at this moment.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be
used to locate the resource, if relative path is given as an argument to this
macro.

Example:
```common-lisp
 (gamekit:define-image 'example-package::logo \"images/logo.png\")
```")


(docstring (macro-function 'define-sound)
  "Registers sound resource by name that can be used by [`#'play-sound`](#gamekit-play-sound) later.
Second argument is a valid path to the resource.  Formats supported: .wav,
.ogg (Vorbis), .flac, .aiff.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be
used to locate the resource, if relative path is given as an argument to this
macro.

Example:
```common-lisp
 (gamekit:define-sound 'example-package::blop \"sounds/blop.ogg\")
```")


(docstring (macro-function 'define-font)
  "Registers font resource by name that can be passed to [`#'make-font`](#gamekit-make-font) later.
Second argument is a valid path to the resource. Only .ttf format is supported
at this moment.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be
used to locate the resource, if relative path is given as an argument to this
macro.

Example:
```common-lisp
 (gamekit:define-font 'example-package::noto-sans \"fonts/NotoSans-Regular.ttf\")
```")


(docstring (macro-function 'define-text)
  "Registers text resource by name that can be retrieved with [`#'get-text`](#gamekit-get-text) later.
Second argument is a valid path to the resource. You can specify encoding via
`:encoding` keywrod argument. `:utf-8` is used by default.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be
used to locate the resource, if relative path is given as an argument to this
macro.

Example:
```common-lisp
 (gamekit:define-text 'example-package::example-text \"dialog.txt\" :encoding :utf-8)
```")


(docstring (macro-function 'define-binary)
  "Registers binary resource by name that can be retrieved with [`#'get-binary`](#gamekit-get-binary) later.
Second argument is a valid path to the resource.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be
used to locate the resource, if relative path is given as an argument to this
macro.

Example:
```common-lisp
 (gamekit:define-binary 'example-package::example-blob \"blob.data\")
```")


(docstring (macro-function 'with-pushed-canvas)
  "Saves current canvas transformations (translations, rotations, scales) before
entering its body and restores previous transformations upon exit from the
body. All transformation operations within this macro don't affect outer canvas
transformations outside of a body of this macro.

Example:
```common-lisp
 (gamekit:translate-canvas 400 300)
 (gamekit:with-pushed-canvas ()
   (gamekit:rotate-canvas (/ pi 4)))
```")


(docstring #'get-text
  "Get text resource (a string) by id. `resource-id` must be a valid resource id
previously registered with [`'define-text`](#gamekit-define-text).

```common-lisp
 (gamekit:get-text 'example-package::example-text)
```")


(docstring #'get-binary
  "Get binary resource (a byte vector) by id. `resource-id` must be a valid
resource id previously registered with [`'define-binary`](#gamekit-define-binary).

```common-lisp
 (gamekit:get-binary 'example-package::example-blob)
```")


(docstring #'deliver
  "Builds an executable, serializes resources and packs required foreign
libraries into a .zip archive for distribution. `system-name` is a name of
`asdf` system of your application and `game-class` is a game class defined with
[`defgame`](#gamekit-defgame) (the one that could be passed to
[`#'start`](#gamekit-start) to start your game). By default, it builds all
artifacts into `build/` directory relative to `system-name` system path, but you
can pass any other path to `:build-directory` key argument to put target files
into it instead.

This routine uses `zip` and `lisp` ('sbcl' [Steel Bank Common
Lisp](http://sbcl.org) is the default) to build a distributable package on
various platforms. If those executables are not on your system's `PATH`, you
would need to provide absolute paths to them via `:zip` and `:lisp` key
arguments accordingly.

You can load this function into an image via `:trivial-gamekit/distribution` system.

Example:
```common-lisp
\(ql:quickload :trivial-gamekit/distribution)
\(gamekit.distribution:deliver :example-asdf-system 'example-package::example
                              :build-directory \"/tmp/example-game/\"
                              :zip \"/usr/bin/zip\"
                              :lisp \"/usr/bin/sbcl\")
```")


(docstring #'viewport-width
  "Returns width of a gamekit viewport (window) if there's an active gamekit
instance (started via [`#'start`](#gamekit-start)) or nil otherwise.

Example:

```common-lisp
  (gamekit:viewport-width)
```")


(docstring #'viewport-height
  "Returns height of a gamekit viewport (window) if there's an active gamekit
instance (started via [`#'start`](#gamekit-start)) or nil otherwise.

Example:

```common-lisp
  (gamekit:viewport-height)
```")


(docstring #'image-width
  "Returns width of an image by its id (defined with
 [`#'define-image`](#gamekit-define-image)).

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example:
```common-lisp
 (gamekit:image-width 'example-package::logo)
```")


(docstring #'image-height
  "Returns height of an image by its id (defined with
 [`#'define-image`](#gamekit-define-image)).

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example:
```common-lisp
 (gamekit:image-height 'example-package::logo)
```")


(docstring #'bind-any-gamepad
  "Binds `action` to a gamepad connection and disconnection events. Once one of
those events happen, `action` is called with two arguments: `gamepad` - opaque
reference to a gamepad that will be supplied as an argument to other
gamepad-related actions, and `state` - which can be either `:connected` or
`:disconnected` to catch connection and disconnection of a gamepad accordingly.

If there were gamepads already connected before call to `#'bind-any-gamepad`,
`action` is called for each one of those upon invocation.

Example:
```common-lisp
 (gamekit:bind-any-gamepad (lambda (gamepad state)
                             (if (eq :connected state)
                                 (add-player-for-gamepad gamepad)
                                 (pause-game-and-wait-for-player gamepad))))
```")

(docstring #'bind-gamepad-button
  "Binds `action` to specified gamepad's `button` `state`.  When button state
changes to the one specified, action callback is invoked with gamepad opaque
reference as an argument. `state` can be either `:pressed` or `:released`.

Actions are not stacked together and would be overwritten for the same button and state.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Gamekit's gamepad is a generic xbox controller with the same layout of controls.

Supported buttons:
```common-lisp
  :a :b :x :y
  :left-bumper :right-bumper
  :start :back :guide
  :left-thumb :right-thumb
```

Example
```common-lisp
 (gamekit:bind-gamepad-button :start :pressed
                              (lambda (gamepad)
                                (declare (ignore gamepad))
                                (start-game)))
```")


(docstring #'bind-gamepad-any-button
  "Binds `action` to all buttons of gamepads. When any button state of any
gamepad changes, action callback is invoked with gamepad opaque reference as a
first argument, gamepad's button as a second and button's state as a third argument.
See [`#'bind-gamepad-button`](#gamekit-bind-gamepad-button) for available button
values and states.

Actions are not stacked together and would be overwritten on each function invocation.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example
```common-lisp
 (gamekit:bind-gamepad-any-button (lambda (gamepad button state)
                                    (when (and (eq button :start) (eq state :pressed))
                                      (join-party (make-player-for-gamepad gamepad)))))
```")


(docstring #'bind-gamepad-dpad
  "Binds `action` to gamepad's dpad. When dpad state changes, action callback is
invoked with gamepad opaque reference as a first argument and new dpad state as a
second.

Dpad states:
```common-lisp
  :up :down :left :right
  :left-up :left-down
  :right-up :right-down
  :centered
```

Actions are not stacked together and would be overwritten for the same dpad
state.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example
```common-lisp
 (gamekit:bind-gamepad-state :up (lambda (gamepad)
                                   (declare (ignore gamepad))
                                   (jump *player*)))
```")

(docstring #'bind-gamepad-stick
  "Binds `action` to gamepad's left or right stick. When position of the
specified stick changes, action callback is invoked with `gamepad` opaque
reference as a first argument, position's `x` and `y` as second and third
arguments. `x` and `y` values are in [-1;1] range: stick up (0;1), stick
down (0;-1), stick left (-1;0) and stick right (1;0).

Sticks: `:left` and `:right`.

Actions are not stacked together and would be overwritten for the same stick.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example
```common-lisp
 (gamekit:bind-gamepad-stick :left (lambda (gamepad x y)
                                     (declare (ignore gamepad))
                                     (move-player *player* x y)))
```")

(docstring #'bind-gamepad-trigger
  "Binds `action` to gamepad's left or right triggers. When value of the
specified trigger changes, action callback is invoked with `gamepad` opaque
reference as a first argument and new trigger value as second. Trigger values
are in [0;1] range.

Triggers: `:left` and `:right`.

Actions are not stacked together and would be overwritten for the same trigger.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example
```common-lisp
 (gamekit:bind-gamepad-trigger :right (lambda (gamepad value)
                                        (declare (ignore gamepad))
                                        (setf (shot-power *player*) value)))
```")
