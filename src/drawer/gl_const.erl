-module(gl_const).
-compile(export_all).

-include_lib("wx/include/gl.hrl").

gl_smooth() ->
  ?GL_SMOOTH.

gl_depth_test() ->
  ?GL_DEPTH_TEST.

gl_lequal() ->
  ?GL_LEQUAL.

gl_perspective_correction_hint() ->
  ?GL_PERSPECTIVE_CORRECTION_HINT.

gl_nicest() ->
  ?GL_NICEST.

gl_color_buffer_bit() ->
  ?GL_COLOR_BUFFER_BIT.

gl_depth_buffer_bit() ->
  ?GL_DEPTH_BUFFER_BIT.

gl_triangles() ->
  ?GL_TRIANGLES.

gl_points() ->
  ?GL_POINTS.

gl_projection() ->
  ?GL_PROJECTION.

gl_modelview() ->
  ?GL_MODELVIEW.

gl_array_buffer() ->
  ?GL_ARRAY_BUFFER.

gl_static_draw() ->
  ?GL_STATIC_DRAW.

gl_vertex_array() ->
  ?GL_VERTEX_ARRAY.

gl_float() ->
  ?GL_FLOAT.

gl_dynamic_draw() ->
  ?GL_DYNAMIC_DRAW.

gl_vertex_shader() ->
  ?GL_VERTEX_SHADER.

gl_fragment_shader() ->
  ?GL_FRAGMENT_SHADER.

gl_info_log_length() ->
  ?GL_INFO_LOG_LENGTH.