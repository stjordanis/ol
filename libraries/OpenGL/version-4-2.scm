; OpenGL 4.2 (2011)

(define-library (OpenGL version-4-2)
(export

   GL_VERSION_4_2

   (exports (OpenGL version-4-1)))

(import (scheme core)
   (OpenGL version-4-1))

(begin
   (define GL_VERSION_4_2 1)


))