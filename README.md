# frame-cycle
Cycle frame content between existing emacs frames.

## Interactive functions description:
### frame-cycle-head-to-tail

   moves frames as follow: frame1 frame2 frame3 frame4 -> frame2 frame3 frame4 frame1

### frame-cycle-head-to-tail

   moves frames as follow: frame1 frame2 frame3 frame4 -> frame4 frame1 frame2 frame3

## Configuration sample:

```elisp
(require 'frame-cycle)
(global-set-key (kbd "C-<left>") 'frame-cycle-head-to-tail)   
(global-set-key (kbd "C-<right>") 'frame-cycle-tail-to-head)
 ```
