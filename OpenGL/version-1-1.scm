; OpenGL 1.1 (1997)
; в комментриях указаны расширения, которые были интегрированы в этот выпуск OpenGL
; todo: во время загрузки расширения проверять, действительно ли оно существует
;	и соответственно этому себя вести

; ===========================================================================
; EXT_vertex_array
;	Multiple vertices may be passed to the GL with a single function call.
;
;	https://www.opengl.org/registry/specs/EXT/vertex_array.txt
;
; Version
;	$Date: 1995/10/03 05:39:58 $ $Revision: 1.16 $  FINAL
;
; Overview
;	This extension adds the ability to specify multiple geometric primitives
;	with very few subroutine calls.  Instead of calling an OpenGL procedure
;	to pass each individual vertex, normal, or color, separate arrays
;	of vertexes, normals, and colors are prespecified, and are used to
;	define a sequence of primitives (all of the same type) when a single
;	call is made to DrawArraysEXT.  A stride mechanism is provided so that
;	an application can choose to keep all vertex data staggered in a
;	single array, or sparsely in separate arrays.  Single-array storage
;	may optimize performance on some implementations.
;
;	This extension also supports the rendering of individual array elements,
;	each specified as an index into the enabled arrays.
(define-library (OpenGL EXT vertex_array)

; ---------------------------------------------------------------------------
; Dependencies
;	None
   (import
      (r5rs base) (owl io)
      (owl pinvoke)
      (OpenGL version-1-0))

; ---------------------------------------------------------------------------
   (export
    EXT_vertex_array

; ---------------------------------------------------------------------------
; New Procedures and Functions
   ;ArrayElementEXT
   ;DrawArraysEXT    
   ;VertexPointerEXT
   ;NormalPointerEXT
   ;ColorPointerEXT
   ;IndexPointerEXT
   ;TexCoordPointerEXT
   ;EdgeFlagPointerEXT
   ;GetPointervEXT
   
; ---------------------------------------------------------------------------
; New Tokens
;
;	Accepted by the <cap> parameter of Enable, Disable, and IsEnabled, and
;	by the <pname> parameter of GetBooleanv, GetIntegerv, GetFloatv, and
;	GetDoublev:
;        VERTEX_ARRAY_EXT               0x8074
;        NORMAL_ARRAY_EXT               0x8075
;        COLOR_ARRAY_EXT                0x8076
;        INDEX_ARRAY_EXT                0x8077
;        TEXTURE_COORD_ARRAY_EXT        0x8078
;        EDGE_FLAG_ARRAY_EXT            0x8079

;	Accepted by the <type> parameter of VertexPointerEXT, NormalPointerEXT,
;	ColorPointerEXT, IndexPointerEXT, and TexCoordPointerEXT:
;        DOUBLE_EXT                     0x140A

;	Accepted by the <pname> parameter of GetBooleanv, GetIntegerv,
;	GetFloatv, and GetDoublev:
;        VERTEX_ARRAY_SIZE_EXT          0x807A
;        VERTEX_ARRAY_TYPE_EXT          0x807B
;        VERTEX_ARRAY_STRIDE_EXT        0x807C
;        VERTEX_ARRAY_COUNT_EXT         0x807D
;        NORMAL_ARRAY_TYPE_EXT          0x807E
;        NORMAL_ARRAY_STRIDE_EXT        0x807F
;        NORMAL_ARRAY_COUNT_EXT         0x8080
;        COLOR_ARRAY_SIZE_EXT           0x8081
;        COLOR_ARRAY_TYPE_EXT           0x8082
;        COLOR_ARRAY_STRIDE_EXT         0x8083
;        COLOR_ARRAY_COUNT_EXT          0x8084
;        INDEX_ARRAY_TYPE_EXT           0x8085
;        INDEX_ARRAY_STRIDE_EXT         0x8086
;        INDEX_ARRAY_COUNT_EXT          0x8087
;        TEXTURE_COORD_ARRAY_SIZE_EXT   0x8088
;        TEXTURE_COORD_ARRAY_TYPE_EXT   0x8089
;        TEXTURE_COORD_ARRAY_STRIDE_EXT 0x808A
;        TEXTURE_COORD_ARRAY_COUNT_EXT  0x808B
;        EDGE_FLAG_ARRAY_STRIDE_EXT     0x808C
;        EDGE_FLAG_ARRAY_COUNT_EXT      0x808D
        
;	Accepted by the <pname> parameter of GetPointervEXT:
;        VERTEX_ARRAY_POINTER_EXT       0x808E
;        NORMAL_ARRAY_POINTER_EXT       0x808F
;        COLOR_ARRAY_POINTER_EXT        0x8090
;        INDEX_ARRAY_POINTER_EXT        0x8091
;        TEXTURE_COORD_ARRAY_POINTER_EXT 0x8092
;        EDGE_FLAG_ARRAY_POINTER_EXT    0x8093

)

; ---------------------------------------------------------------------------
   (begin
   (define EXT_vertex_array 1)
   
))


; ===========================================================================
; EXT_polygon_offset
;	Depth values may be offset on a per-primitive basis.
;
;	https://www.opengl.org/registry/specs/EXT/polygon_offset.txt
;
; Version
;	$Date: 1995/06/17 03:34:49 $ $Revision: 1.12 $
;
; Overview
;	The depth values of fragments generated by rendering polygons are
;	displaced by an amount that is proportional to the maximum absolute
;	value of the depth slope of the polygon, measured and applied in window
;	coordinates.  This displacement allows lines (or points) and polygons
;	in the same plane to be rendered without interaction -- the lines
;	rendered either completely in front of or behind the polygons
;	(depending on the sign of the offset factor).  It also allows multiple
;	coplanar polygons to be rendered without interaction, if different
;	offset factors are used for each polygon.  Applications include
;	rendering hidden-line images, rendering solids with highlighted edges,
;	and applying `decals' to surfaces.
(define-library (OpenGL EXT polygon_offset)

; ---------------------------------------------------------------------------
; Dependencies
;	None
   (import
      (r5rs base) (owl io)
      (owl pinvoke)
      (OpenGL version-1-0))

; ---------------------------------------------------------------------------
   (export
    EXT_polygon_offset
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
   (begin
   (define EXT_polygon_offset 1)
   
))


; ===========================================================================
; EXT_blend_logic_op
;	Fragment colors may be blended into the framebuffer using bitwise operations.
;
;	https://www.opengl.org/registry/specs/EXT/blend_logic_op.txt
;
; Version
;	 $Date: 1995/03/31 04:40:24 $ $Revision: 1.4 $
;
; Overview
;	A single additional blending equation is specified using the interface
;	defined by EXT_blend_minmax.  This equation is a simple logical
;	combination of the source and destination colors, where the specific
;	logical operation is as specified by LogicOp.  While only the XOR
;	operation may find wide application, the generality of full logical
;	operations is allowed.
(define-library (OpenGL EXT blend_logic_op)

; ---------------------------------------------------------------------------
; Dependencies
;	EXT_blend_minmax affects the definition of this extension
   (import
      (r5rs base) (owl io)
      (owl pinvoke)
      (OpenGL version-1-0))
      
; ---------------------------------------------------------------------------
   (export
    EXT_blend_logic_op
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens
    
)
  
; ---------------------------------------------------------------------------
   (begin
   (define EXT_blend_logic_op 1)
  
))


; ===========================================================================
; EXT_texture
;	Various texturing improvements, including proxy textures and sized internal formats.
;
;	http://oss.sgi.com/projects/ogl-sample/registry/EXT/texture.txt
;	https://www.opengl.org/registry/specs/EXT/texture.txt
;
; Version
;	$Date: 1996/04/05 19:17:03 $ $Revision: 1.21 $
;
; Overview
;	The original intention of this extension was simply to support various
;	numeric resolutions of color components in texture images.  While it
;	accomplishes this, it also accomplishes a larger task, that of
;	formalizing the notion of an internal format for images, corresponding
;	to the external format that already existed for image data in host
;	memory.  This notion of an internal image format will be used
;	extensively in later extensions, especially those concerned with pixel
;	manipulation.
;
;	The idea of an internal format is simple: rather than treating a
;	retained image as having 1, 2, 3, or 4 components, treat it as though
;	it has a specific format, such as LUMINANCE_ALPHA, or just ALPHA.  Then
;	define the semantics of the use of internal images with these formats in
;	a consistent way.  Because texture mapping is already defined in GL, the
;	semantics for internal-format images were chosen to match those of the 1,
;	2, 3, and 4 component internal images that already existed.  The new
;	semantics are a superset of the old ones, however, so this extension
;	adds capabilities to GL, as well as allowing internal resolutions to be
;	specified.
;
;	This extension also defines a robust method for applications to
;	determine what combinations of texture dimensions and resolutions are
;	supported by an implementation.  It also introduces a new texture
;	environment: REPLACE_EXT.
(define-library (OpenGL EXT texture)

; ---------------------------------------------------------------------------
; Dependencies
;	None
   (import
      (r5rs base) (owl io)
      (owl pinvoke)
      (OpenGL version-1-0))
      
; ---------------------------------------------------------------------------
   (export
    EXT_texture
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens
    
)
  
; ---------------------------------------------------------------------------
   (begin
   (define EXT_texture 1)
   
))


; ===========================================================================
; EXT_copy_texture
;	Various methods to alter texture images, including image copying and sub-image replacement.
;
;	https://www.opengl.org/registry/specs/EXT/copy_texture.txt
;
; Version
;	$Date: 1995/06/17 03:33:42 $ $Revision: 1.21 $
;
; Overview
;	This extension defines methods to load texture images directly from the
;	framebuffer.  Methods are defined for both complete and partial
;	replacement of a texture image.  Because it is not possible to define
;	an entire 3D texture using a 2D framebuffer image, 3D textures are
;	supported only for partial replacement.
(define-library (OpenGL EXT copy_texture)

; ---------------------------------------------------------------------------
; Dependencies
;	EXT_texture3D affects the definition of this extension.
;	SGIS_texture_filter4 affects the definition of this extension.
;	EXT_subtexture affects the definition of this extension.
   (import
      (r5rs base) (owl io)
      (owl pinvoke)
      (OpenGL version-1-0))
;	EXT_texture is required.
   (import
      (OpenGL EXT texture))

; ---------------------------------------------------------------------------
   (export
    EXT_copy_texture
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens
    
)
  
; ---------------------------------------------------------------------------
   (begin
   (define EXT_copy_texture 1)
   
))


; ===========================================================================
; EXT_subtexture
;	Various methods to alter texture images, including image copying and sub-image replacement.
;
;	https://www.opengl.org/registry/specs/EXT/subtexture.txt
;
; Version
;	$Date: 1995/10/03 05:39:55 $ $Revision: 1.17 $
;
; Overview
;	This extension allows a contiguous portion of an already-existing
;	texture image to be redefined, without affecting the remaining portion
;	of the image, or any of the other state that describe the texture.  No
;	provision is made to query a subregion of a texture.

;	Semantics for null image pointers are defined for TexImage1D,
;	TexImage2D, and TexImage3DEXT.  Null image pointers can be used by
;	applications to effectively support texture arrays whose dimensions
;	are not a power of 2.
(define-library (OpenGL EXT subtexture)

; ---------------------------------------------------------------------------
; Dependencies
;	EXT_abgr affects the definition of this extension
;	EXT_texture3D affects the definition of this extension
   (import
      (r5rs base) (owl io)
      (owl pinvoke)
      (OpenGL version-1-0))
;	EXT_texture is required.
   (import
      (OpenGL EXT texture))

; ---------------------------------------------------------------------------
   (export
    EXT_subtexture
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens
    
)
  
; ---------------------------------------------------------------------------
   (begin
   (define EXT_subtexture 1)
   
))


; ===========================================================================
; EXT_texture_object
;	Texture state may be stored in a GL object, for greater efficiency.
;
;	https://www.opengl.org/registry/specs/EXT/texture_object.txt
;
; Version
;	$Date: 1995/10/03 05:39:56 $ $Revision: 1.27 $
;
; Overview
;	This extension introduces named texture objects.  The only way to name
;	a texture in GL 1.0 is by defining it as a single display list.  Because
;	display lists cannot be edited, these objects are static.  Yet it is
;	important to be able to change the images and parameters of a texture.
(define-library (OpenGL EXT texture_object)

; ---------------------------------------------------------------------------
; Dependencies
;	EXT_texture3D affects the definition of this extension
   (import
      (r5rs base) (owl io)
      (owl pinvoke)
      (OpenGL version-1-0))

; ---------------------------------------------------------------------------
   (export
    EXT_texture_object

; ---------------------------------------------------------------------------
; New Procedures and Functions
      glGenTextures ; void (GLsizei n, GLuint *textures)
      ;DeleteTextures
      glBindTexture ; void (GLenum target, GLuint texture)
      ;PrioritizeTextures
      ;AreTexturesResident
      ;IsTexture

; ---------------------------------------------------------------------------
; New Tokens
    
)
  
; ---------------------------------------------------------------------------
   (begin
   (define EXT_texture_object 1) ; todo: change to the dynamic, maybe.
   (define % (dlopen GL_LIBRARY 0))

   (define glBindTexture (dlsym % GLvoid "glBindTexture" GLenum GLuint))
   ;WINGDIAPI void APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
   (define glGenTextures (dlsym % GLvoid "glGenTextures" GLsizei GLuint*))
   ;WINGDIAPI GLboolean APIENTRY glIsTexture (GLuint texture);
   
))


; ===========================================================================
; ===========================================================================
(define-library (OpenGL version-1-1)
   (export
      (exports (OpenGL version-1-0))
    GL_VERSION_1_1
      
      (exports (OpenGL EXT vertex_array))
      (exports (OpenGL EXT polygon_offset))
      (exports (OpenGL EXT blend_logic_op))
      (exports (OpenGL EXT texture))
      (exports (OpenGL EXT copy_texture))
      (exports (OpenGL EXT subtexture))
      (exports (OpenGL EXT texture_object))
      
    
    ; todo: move this to the right place
      GL_ALPHA4
      GL_ALPHA8
      GL_ALPHA12
      GL_ALPHA16
      GL_LUMINANCE4
      GL_LUMINANCE8
      GL_LUMINANCE12
      GL_LUMINANCE16
      GL_LUMINANCE4_ALPHA4
      GL_LUMINANCE6_ALPHA2
      GL_LUMINANCE8_ALPHA8
      GL_LUMINANCE12_ALPHA4
      GL_LUMINANCE12_ALPHA12
      GL_LUMINANCE16_ALPHA16
      GL_INTENSITY
      GL_INTENSITY4
      GL_INTENSITY8
      GL_INTENSITY12
      GL_INTENSITY16
      ;GL_RGB2
      GL_RGB4
      GL_RGB5
      GL_RGB8
      GL_RGB10
      GL_RGB12
      GL_RGB16
      GL_RGBA2
      GL_RGBA4
      GL_RGB5_A1
      GL_RGBA8
      GL_RGB10_A2
      GL_RGBA12
      GL_RGBA16

      GL_TEXTURE_RED_SIZE
      GL_TEXTURE_GREEN_SIZE
      GL_TEXTURE_BLUE_SIZE
      GL_TEXTURE_ALPHA_SIZE
      GL_TEXTURE_LUMINANCE_SIZE
      GL_TEXTURE_INTENSITY_SIZE
;WINGDIAPI void APIENTRY glDrawArrays (GLenum mode, GLint first, GLsizei count);
;WINGDIAPI void APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const GLvoid *indices);
;WINGDIAPI void APIENTRY glGetPointerv (GLenum pname, GLvoid* *params);
;WINGDIAPI void APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
;WINGDIAPI void APIENTRY glCopyTexImage1D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLint border);
;WINGDIAPI void APIENTRY glCopyTexImage2D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
;WINGDIAPI void APIENTRY glCopyTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width);
;WINGDIAPI void APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
;WINGDIAPI void APIENTRY glTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
;WINGDIAPI void APIENTRY glGenTextures (GLsizei n, GLuint *textures);
;WINGDIAPI GLboolean APIENTRY glIsTexture (GLuint texture);
;WINGDIAPI void APIENTRY glArrayElement (GLint i);
;WINGDIAPI void APIENTRY glColorPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glDisableClientState (GLenum array);
;WINGDIAPI void APIENTRY glEdgeFlagPointer (GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glEnableClientState (GLenum array);
;WINGDIAPI void APIENTRY glIndexPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glInterleavedArrays (GLenum format, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glNormalPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glTexCoordPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glVertexPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI GLboolean APIENTRY glAreTexturesResident (GLsizei n, const GLuint *textures, GLboolean *residences);
;WINGDIAPI void APIENTRY glPrioritizeTextures (GLsizei n, const GLuint *textures, const GLclampf *priorities);
;WINGDIAPI void APIENTRY glIndexub (GLubyte c);
;WINGDIAPI void APIENTRY glIndexubv (const GLubyte *c);
;WINGDIAPI void APIENTRY glPopClientAttrib (void);
;WINGDIAPI void APIENTRY glPushClientAttrib (GLbitfield mask);

)

; ---------------------------------------------------------------------------
   (import
      (r5rs base) (owl io)
      (owl pinvoke)
      (OpenGL version-1-0))

   (import (OpenGL EXT vertex_array))
   (import (OpenGL EXT polygon_offset))
   (import (OpenGL EXT blend_logic_op))
   (import (OpenGL EXT texture))
   (import (OpenGL EXT copy_texture))
   (import (OpenGL EXT subtexture))
   (import (OpenGL EXT texture_object))

; ---------------------------------------------------------------------------
   (begin
   (define GL_VERSION_1_1 1)

; 1.1
;(define GLbyte  ?) ; typedef signed char GLbyte
;(define GLshort ?) ; typedef short GLshort
;(define GLushort ?); typedef unsigned short GLushort
;typedef float GLclampf
;typedef double GLclampd

		(define GL_DOUBLE #x140A)
		(define GL_VERTEX_ARRAY #x8074)
		(define GL_NORMAL_ARRAY #x8075)
		(define GL_COLOR_ARRAY #x8076)
		(define GL_INDEX_ARRAY #x8077)
		(define GL_TEXTURE_COORD_ARRAY #x8078)
		(define GL_EDGE_FLAG_ARRAY #x8079)
		(define GL_VERTEX_ARRAY_SIZE #x807A)
		(define GL_VERTEX_ARRAY_TYPE #x807B)
		(define GL_VERTEX_ARRAY_STRIDE #x807C)
		(define GL_NORMAL_ARRAY_TYPE #x807E)
		(define GL_NORMAL_ARRAY_STRIDE #x807F)
		(define GL_COLOR_ARRAY_SIZE #x8081)
		(define GL_COLOR_ARRAY_TYPE #x8082)
		(define GL_COLOR_ARRAY_STRIDE #x8083)
		(define GL_INDEX_ARRAY_TYPE #x8085)
		(define GL_INDEX_ARRAY_STRIDE #x8086)
		(define GL_TEXTURE_COORD_ARRAY_SIZE #x8088)
		(define GL_TEXTURE_COORD_ARRAY_TYPE #x8089)
		(define GL_TEXTURE_COORD_ARRAY_STRIDE #x808A)
		(define GL_EDGE_FLAG_ARRAY_STRIDE #x808C)
		(define GL_VERTEX_ARRAY_POINTER #x808E)
		(define GL_NORMAL_ARRAY_POINTER #x808F)
		(define GL_COLOR_ARRAY_POINTER #x8090)
		(define GL_INDEX_ARRAY_POINTER #x8091)
		(define GL_TEXTURE_COORD_ARRAY_POINTER #x8092)
		(define GL_EDGE_FLAG_ARRAY_POINTER #x8093)
		(define GL_V2F #x2A20)
		(define GL_V3F #x2A21)
		(define GL_C4UB_V2F #x2A22)
		(define GL_C4UB_V3F #x2A23)
		(define GL_C3F_V3F #x2A24)
		(define GL_N3F_V3F #x2A25)
		(define GL_C4F_N3F_V3F #x2A26)
		(define GL_T2F_V3F #x2A27)
		(define GL_T4F_V4F #x2A28)
		(define GL_T2F_C4UB_V3F #x2A29)
		(define GL_T2F_C3F_V3F #x2A2A)
		(define GL_T2F_N3F_V3F #x2A2B)
		(define GL_T2F_C4F_N3F_V3F #x2A2C)
		(define GL_T4F_C4F_N3F_V4F #x2A2D)
		(define GL_POLYGON_OFFSET #x8037)
		(define GL_POLYGON_OFFSET_FACTOR #x8038)
		(define GL_POLYGON_OFFSET_UNITS #x2A00)
		(define GL_POLYGON_OFFSET_POINT #x2A01)
		(define GL_POLYGON_OFFSET_LINE #x2A02)
		(define GL_POLYGON_OFFSET_FILL #x8037)
		(define GL_ALPHA4 #x803B)
		(define GL_ALPHA8 #x803C)
		(define GL_ALPHA12 #x803D)
		(define GL_ALPHA16 #x803E)
		(define GL_LUMINANCE4 #x803F)
		(define GL_LUMINANCE8 #x8040)
		(define GL_LUMINANCE12 #x8041)
		(define GL_LUMINANCE16 #x8042)
		(define GL_LUMINANCE4_ALPHA4 #x8043)
		(define GL_LUMINANCE6_ALPHA2 #x8044)
		(define GL_LUMINANCE8_ALPHA8 #x8045)
		(define GL_LUMINANCE12_ALPHA4 #x8046)
		(define GL_LUMINANCE12_ALPHA12 #x8047)
		(define GL_LUMINANCE16_ALPHA16 #x8048)
		(define GL_INTENSITY #x8049)
		(define GL_INTENSITY4 #x804A)
		(define GL_INTENSITY8 #x804B)
		(define GL_INTENSITY12 #x804C)
		(define GL_INTENSITY16 #x804D)
		(define GL_R3_G3_B2 #x2A10)
		(define GL_RGB4 #x804F)
		(define GL_RGB5 #x8050)
		(define GL_RGB8 #x8051)
		(define GL_RGB10 #x8052)
		(define GL_RGB12 #x8053)
		(define GL_RGB16 #x8054)
		(define GL_RGBA2 #x8055)
		(define GL_RGBA4 #x8056)
		(define GL_RGB5_A1 #x8057)
		(define GL_RGBA8 #x8058)
		(define GL_RGB10_A2 #x8059)
		(define GL_RGBA12 #x805A)
		(define GL_RGBA16 #x805B)
		(define GL_TEXTURE_RED_SIZE #x805C)
		(define GL_TEXTURE_GREEN_SIZE #x805D)
		(define GL_TEXTURE_BLUE_SIZE #x805E)
		(define GL_TEXTURE_ALPHA_SIZE #x805F)
		(define GL_TEXTURE_LUMINANCE_SIZE #x8060)
		(define GL_TEXTURE_INTENSITY_SIZE #x8061)
		(define GL_PROXY_TEXTURE_1D #x8063)
		(define GL_PROXY_TEXTURE_2D #x8064)
		(define GL_TEXTURE_TOO_LARGE #x8065)
		(define GL_TEXTURE_PRIORITY #x8066)
		(define GL_TEXTURE_RESIDENT #x8067)
		(define GL_TEXTURE_BINDING_1D #x8068)
		(define GL_TEXTURE_BINDING_2D #x8069)
		(define GL_CLIENT_PIXEL_STORE_BIT #x00000001)
		(define GL_CLIENT_VERTEX_ARRAY_BIT #x00000002)
		(define GL_CLIENT_ALL_ATTRIB_BITS #xFFFFFFFF)

;WINGDIAPI void APIENTRY glDrawArrays (GLenum mode, GLint first, GLsizei count);
;WINGDIAPI void APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const GLvoid *indices);
;WINGDIAPI void APIENTRY glGetPointerv (GLenum pname, GLvoid* *params);
;WINGDIAPI void APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
;WINGDIAPI void APIENTRY glCopyTexImage1D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLint border);
;WINGDIAPI void APIENTRY glCopyTexImage2D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
;WINGDIAPI void APIENTRY glCopyTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width);
;WINGDIAPI void APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
;WINGDIAPI void APIENTRY glTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glArrayElement (GLint i);
;WINGDIAPI void APIENTRY glColorPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glDisableClientState (GLenum array);
;WINGDIAPI void APIENTRY glEdgeFlagPointer (GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glEnableClientState (GLenum array);
;WINGDIAPI void APIENTRY glIndexPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glInterleavedArrays (GLenum format, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glNormalPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glTexCoordPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glVertexPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI GLboolean APIENTRY glAreTexturesResident (GLsizei n, const GLuint *textures, GLboolean *residences);
;WINGDIAPI void APIENTRY glPrioritizeTextures (GLsizei n, const GLuint *textures, const GLclampf *priorities);
;WINGDIAPI void APIENTRY glIndexub (GLubyte c);
;WINGDIAPI void APIENTRY glIndexubv (const GLubyte *c);
;WINGDIAPI void APIENTRY glPopClientAttrib (void);
;WINGDIAPI void APIENTRY glPushClientAttrib (GLbitfield mask);

))