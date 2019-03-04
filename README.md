# frame-cycle
Cycle frame content between existing emacs frames.
Better to run on emacs 26 to take benefits of frame-restack function.

## Interactive functions description:
### frame-cycle-next

   moves frames as follow: frame1 frame2 frame3 frame4 -> frame2 frame3 frame4 frame1

### frame-cycle-prev

   moves frames as follow: frame1 frame2 frame3 frame4 -> frame4 frame1 frame2 frame3

## Configuration sample:

```elisp
(require 'frame-cycle)
(global-set-key (kbd "C-<right>") 'frame-cycle-next)
(global-set-key (kbd "C-<left>" ) 'frame-cycle-prev)   
 ```
