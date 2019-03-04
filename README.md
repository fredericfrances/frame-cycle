# frame-cycle

Cycle frame position, keep focuse on position occupied by original top frame.

Known issue: when cycling to fast, focus moves to another frame.

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
