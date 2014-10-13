;;; humanely-sort.el --- sort lists humanely
;; Copyright (C) 2014 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions

;; humanely-sort.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; humanely-sort.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defun humanely-sort (list &optional accessor)
  "Sort LIST humanely.
LIST can either be a list of strings, or a list of other types of
elements where ACCESSOR is called to return a string."
  (sort list (lambda (e1 e2)
	       (if accessor
		   (humane-compare (funcall accessor e1)
				   (funcall accessor e2))
		 (humane-compare e1 e2)))))

(defun humane-compare (string1 string2)
  (let ((list1 (humane-transform string1))
	(list2 (humane-transform string2))
	c1 c2
	(result :unfound))
    (while (and list1 list2
		(eq result :unfound))
      (setq c1 (pop list1)
	    c2 (pop list2))
      (cond
       ((and (numberp c1)
	     (stringp c2))
	(setq result nil))
       ((and (stringp c1)
	     (numberp c2))
	(setq result t))
       ((and (numberp c1)
	     (numberp c2)
	     (not (= c1 c2)))
	(setq result (< c1 c2)))
       ((and (stringp c1)
	     (stringp c2)
	     (not (equal c1 c2)))
	(setq result (string< c1 c2)))))
    (if (eq result :unfound)
	(null list1)
      result)))

(defun humane-transform (string)
  (let ((result nil))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at "[0-9]+")
	    (progn
	      (push (string-to-number (match-string 0)) result)
	      (goto-char (match-end 0)))
	  (push (buffer-substring (point) (1+ (point))) result)
	  (forward-char 1)))
      (nreverse result))))

(provide 'humanely-sort)

;;; humanely-sort.el ends here
