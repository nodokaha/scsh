;; modules.scm -- Module definitions for pg.scm
;;
;;     Copyright (C) 1999  Eric Marsden
;;   
;;     This library is free software; you may redistribute it and/or
;;     modify it under the terms of the GNU Library General Public
;;     License as published by the Free Software Foundation; either
;;     version 2 of the License, or (at your option) any later version.
;;   
;;     This library is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;     Library General Public License for more details.
;;   
;;     You should have received a copy of the GNU Library General Public
;;     License along with this library; if not, write to the Free
;;     Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Please send suggestions and bug reports to <emarsden@mail.dotcom.fr>

(define-structure pg
  (export pg:connect
          pg:exec
          pg:result
          pg:disconnect
          pg:lo-create
          pg:lo-open
          pg:lo-close
          pg:lo-read
          pg:lo-write
          pg:lo-lseek
          pg:lo-tell
          pg:lo-unlink
          pg:lo-import
          pg:lo-export
          pg:databases
          pg:tables
          pg:columns)

  (open defrec-package
        let-opt
        scsh
        scheme)
  (files pg))

;; EOF
