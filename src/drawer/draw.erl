%%%-----------------------------------------------------------------------------
%%% @doc
%%% Empty Module built from template.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(draw).
-author("nickolaswood").

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 
-include_lib("wx/include/glu.hrl"). 

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    start_link/0
]).

%% Callbacks
-export([
    init/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_event/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).
-define(SIZE, {800, 800}).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
    wx_object:start_link(?SERVER, [], []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
init([]) ->
    WX = wx:new(),
    Frame = wxFrame:new(WX, wx_const:wx_id_any(), "Hello", [{size, ?SIZE}]),
    wxFrame:connect(Frame, size),
    wxWindow:connect(Frame, close_window),
    wxFrame:show(Frame),

    Opts = [{style, ?wxFULL_REPAINT_ON_RESIZE}],
    GLAttrib = [{attribList, [?WX_GL_RGBA,
			      ?WX_GL_DOUBLEBUFFER,
			      ?WX_GL_MIN_RED,8,
			      ?WX_GL_MIN_GREEN,8,
			      ?WX_GL_MIN_BLUE,8,
			      ?WX_GL_DEPTH_SIZE,24,0]}],
    Canvas = wxGLCanvas:new(Frame ,Opts ++ GLAttrib),
    Context = wxGLContext:new(Canvas, [{size, ?SIZE}]),

    wxGLCanvas:connect(Canvas, size),
    wxWindow:reparent(Canvas, Frame),
    wxGLCanvas:setCurrent(Canvas, Context),
    
    setup_gl(Canvas),

    wx:batch(fun() ->
        draw([{0.5,0.5}, {20.0,20.0}, {40.0,40.0}, {60.0,60.0}, {10.0,10.0}]),
        wxGLCanvas:swapBuffers(Canvas)
    end),

    {Frame, #{canvas => Canvas}}.

code_change(_, _, State) ->
    {stop, not_implemented, State}.

handle_cast(Msg, State) ->
    io:format("handle_cast: ~p~n", [Msg]),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    io:format("handle_call: ~p~n", [Msg]),
    {reply, ok, State}.

handle_info(Msg, State) ->
    io:format("handle_info: ~p~n", [Msg]),
    {noreply, State}.

handle_event({wx, _, _, _, {wxSize, size, {Width, Height}, _}}, State) ->
    case Width =/= 0 andalso Height =/= 0 of
        true -> ok %resize_gl_scene(width, height)
    end,
    {noreply, State};
handle_event({wx, _, _, _, {wxClose, close_window}}, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    timer:sleep(300).

%%%=============================================================================
%%% Private functions
%%%=============================================================================

setup_gl(Win) ->
    {W, H} = wxWindow:getClientSize(Win),
    resize_gl_scene(800, 800),
    gl:shadeModel(gl_const:gl_smooth()),
    gl:clearColor(0.1569, 0.1647, 0.2118, 1.0),
    gl:clearDepth(1.0),
    gl:enable(gl_const:gl_depth_test()),
    gl:depthFunc(gl_const:gl_lequal()),
    gl:hint(gl_const:gl_perspective_correction_hint(), gl_const:gl_nicest()),
    gl:pointSize(2.0),
    ok.

draw(Points) ->
    gl:clear(gl_const:gl_color_buffer_bit() bor gl_const:gl_depth_buffer_bit()),
    gl:loadIdentity(),
    gl:'begin'(gl_const:gl_points()),
    gl:color4f(0.9725, 0.9725, 0.949, 1.0),
    lists:foreach(
        fun draw_boid/1,
        Points),
    gl:'end'(),
    ok.

draw_boid({X, Y}) ->
    gl:vertex2f(X, Y).

resize_gl_scene(Width, Height) ->
    gl:viewport(0, 0, Width, Height),
    gl:matrixMode(gl_const:gl_projection()),
    gl:loadIdentity(),
    glu:perspective(45.0, Width / Height, 0.1, 100.0),
    gl:matrixMode(gl_const:gl_modelview()),
    gl:loadIdentity(),
    ok.

%   defp render(%{canvas: canvas, boids: boids} = _state) do
%     draw(boids)
%     :wxGLCanvas.swapBuffers(canvas)
%     :ok
%   end