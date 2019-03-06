# frame-cycle

Cycle frame position, keep focus on position occupied by original top frame.

Known issues:
   - this is slow
   - when cycling to fast, focus might moves to another frame.
   - fullscreen may create weird behaviors

## Interactive functions description:

### frame-cycle-next

   moves frames as follow:  frame1 frame2 frame3 frame4 -> frame2 frame3 frame4 frame1

### frame-cycle-prev

   moves frames as follow:  frame1 frame2 frame3 frame4 -> frame4 frame1 frame2 frame3

### frame-cycle-select-frame

    select a frame to swap with.

## Configuration sample:

```elisp
(require 'frame-cycle)
(global-set-key (kbd "<f1> n") 'frame-cycle-next)
(global-set-key (kbd "<f1> p" ) 'frame-cycle-prev)
(global-set-key (kbd "<f1> s" ) 'frame-cycle-select-frame)
 ```
