(ql:quickload :com.informatimago.objcl)

(defpackage "COCOA-HOWTO"
  (:use "CL"))
(in-package "COCOA-HOWTO")

(objcl:set-objective-cl-syntax)

(defun create-window (rect title)
  (let ((window  [[NSWindow alloc]
                  initWithContentRect:rect
                  styleMask:(logior  #$NSTitledWindowMask 
                                     #$NSClosableWindowMask  
                                     #$NSMiniaturizableWindowMask 
                                     #$NSResizableWindowMask)
                  backing:#$NSBackingStoreBuffered
                  defer:nil]))
    [window setTitle:title]
    window))

(defun create-button (rect title)
  (let ((button [[NSButton alloc] initWithFrame:rect]))
    [button setButtonType:#$NSMomentaryPushInButton]
    [button setImagePosition:#$NSNoImage]
    [button setBezelStyle:#$NSRoundedBezelStyle]
    [button setTitle:title]
    button))


(defvar *my-window* (create-window (ns:make-ns-rect 100 100 400 300) @"Hello World"))
(defvar *my-button* (create-button (ns:make-ns-rect 10 50 72 32) @"Hello"))
[*my-window* makeKeyAndOrderFront:nil]
[[*my-window* contentView] addSubview:*my-button*]

@[NSObject subClass:Greeter slots: ()]
@[Greeter method:(greet:(:id)sender)
          resultType:(:id)
          body:
          (declare (ignore sender))
          (#_NSRunAlertPanel @"Hello!"
                             @"Hello, World!"
                             @"Hi!"
                             @"Hello!"
                             @"Go Away")
          oclo:*null*]


(defvar *my-greeter* [[Greeter alloc] init])
[*my-button* setTarget:*my-greeter*]
[*my-button* setAction:(objc:@selector "greet:")]
 

;;;; THE END ;;;;
