# frame-cycle

Cycle frame position, keep focus on position occupied by original top frame.

Known issues:
   - this is slow
   - when cycling to fast, focus might moves to another frame.
   - fullscreen may create weird behaviors

## Interactive functions description:

### frame-cycle-next

   moves frames as follow:
         frame1 frame2 frame3 frame4 -> frame2 frame3 frame4 frame1

### frame-cycle-prev

   moves frames as follow:
         frame1 frame2 frame3 frame4 -> frame4 frame1 frame2 frame3

## Configuration sample:

```elisp
(require 'frame-cycle)
(global-set-key (kbd "C-<right>") 'frame-cycle-next)
(global-set-key (kbd "C-<left>" ) 'frame-cycle-prev)
 ```
