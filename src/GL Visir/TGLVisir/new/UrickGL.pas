{this unit has a group of useful routines for use with OpenGL}
{  TsceneGL: a 3D scene, has a group of entities and a group of lights.
   Tlight: a light, has color, position and orientation.
   T3dMouse: enables a normal mouse to interact easily with one 3D object

   By: Ricardo Sarmiento
       delphiman@hotmail.com
}
unit UrickGL;

interface

Uses Windows, Messages, Dialogs, SysUtils, Classes, GL, GLU, U3DPolys;

Const
  CLSpot=1;         {a spot light, with position and direction, like a flashlamp}
  CLambiental=2;    {an ambiental light, like indirect ilumination}
  CLstar=3;         {a light with position but omnidirectional, like a star}
  CLconstant=1;     {constant light attenuation}
  CLlinear=2;       {linear light attenuation}
  CLquadratic=3;    {quadratic light attenuation}

type
  Tcamera=class(Tobject)
    Position:Array[1..3] of GLdouble;     {position of the camera,  x,y,z}
    SceneCenter:Array[1..3] of GLdouble;  {position for the center of the scene}
    UpVector:Array[1..3] of GLdouble;     {vector pointing "up" from the viewer´s perspective}
    Constructor create;              {creates a new camera}
    Procedure   Redraw;                {redraw camera}
    Procedure   SetPosition(ix,iy,iz:GlDouble);
    Procedure   LookAt(ix,iy,iz:GlDouble);
    Procedure   SetVectorUp(ix,iy,iz:GlDouble);
  end; {Tcamera}

  TsceneGL=class(Tobject)
    DC:HDC;            {this is the Device Context}
    HRC: HGLRC;        {somewhat private}
    WindowHandle:Thandle;    {handle to the host window, a Tpanel most times}
    Entities:Tlist;  {Available entities in the scene}
    Lights:Tlist;    {Available lights in the scene}
    Cameras:Tlist;   {Available cameras in the scene}
    Active:boolean;  {true: InitRC has been called already}
    Fperspective:boolean;  {true: use perspective projection,  false: use orthogonal projection}
    Angle,DistNear,DistFar:single;  {values for Field-of-view angle, distance to near clipping plane, distance to far clipping plane}
    texturing:boolean;    {true: use textures and texture information, false: don´t use textures}
    BackR,BackG,BackB:float; {Background color (RGB), between 0.0 and 1.0 }
    fogColor:array [0..3] of GlFloat; {fog color (RGBA), between 0.0 and 1.0 }
    fogDensity,fogMinDist,fogMaxDist:GlFloat;
    fogEnabled:boolean;
    fogtype:GLint;
    ActiveCamera:Tcamera;   {points to the camera from which the scene is being "filmed"}
    DefaultCamera:Tcamera;  {points to the default camera, this camera shouldn´t never be destroyed}
    bTranslucentMode : boolean; {ignore depth buffer?}
    Constructor create;     {creates a new scene}
    Destructor  destroy; override;   {don´t call this directly, call free instead}

    {initialization of the Rendering Context, receives the handle of the
     display control, frecuently it is a Tpanel}
    Procedure InitRC(iHandle:THandle);
    Procedure Redraw;  {redraw scene}

    {set values for the pixel´s format.  don´t call this Function directly, call
     instead InitRC.}
    Procedure SetDCPixelFormat;

    {Free all resources taken by InitRC.  don´t call this Function directly}
    Procedure ReleaseRC;

    {reflects changes in the width and height (ancho,alto) of the display control}
    Procedure UpdateArea(width,height:integer);

    {Set the perspective values for Field-of-view angle, distance to near clipping plane, distance to far clipping plane}
    Procedure SetPerspective(iAngle,iDistNear,iDistFar:single);

    {Activate a different camera, by number of camera}
    Procedure UseCamera(CameraNumber:integer);
  end; {TsceneGL}

  Tlight=class(Tobject)
    Source:Tvertex;    {position and orientation of the light}
    Fambient,
    Fdiffuse,
    Fspecular:array[0..3] of GLfloat;   {ambient, dIffuse and specular components}
    Number:LongInt;                     {number of the light, from 1 to 8}
    LightType:integer;                  {The type of light, spot, ambiental, omni}
    CutOffAngle:GlFloat;                {the Cut-off angle for spot lights}
    SpotExponent:GlFloat;               {the shininess of the spot light}
    attenuation:GlFloat;                 {attenuation amount}
    attenuationType:integer;             {attenuation type: constant, linear or quadratic}
    Enabled:boolean;                     {indicates if the light is on or off}
    Constructor Create(num:Byte);    {create a new, white Ambient light.  num goes from 1 to 8}
    Destructor  Destroy; override;   {don´t call directly, call instead free}
    Procedure   Ambient(r,g,b,a:GLfloat);    {change the Ambient component of the light}
    Procedure   Diffuse(r,g,b,a:GLfloat);       {change the dIffuse component of the light}
    Procedure   Specular(r,g,b,a:GLfloat);    {change the specular component of the light}
    Procedure   SetOrientation(nx,ny,nz:GlFloat);  {change the direction where the light comes from}
    Procedure   Redraw;                    {executes the OpenGL code for this light}
  end; {Tlight}

  T3dMouse=class(Tobject)    {ease manipulation of 3D objects with the mouse}
    Button1,           {left button on mouse}
    Button2:boolean;   {right button on mouse}
    Mode:integer;      {movement mode:  1-move x,y-z, 2-turn rx,ry-rz
                        3-turn rx,ry-rz+move z.}
    Entity:Tentity;    {points to the entity to be modIfied, just one at a time}
    Start:array[1..6] of single;         {x,y,z - rx,ry,rz   saves the initial position when dragging mouse}
    Scaling:array[1..6] of single;       {x,y,z - rx,ry,rz   stores the speed relations between mouse and 3D}
    BlockStatus:array[1..6] of boolean;  {x,y,z - rx,ry,rz   which movements are blocked}
    Constructor Create(iEntity:Tentity);  {iEntity is the pointer to one entity in the scene}
    Procedure   Move(x,y:single;Botones: TShIftState);  {move or turn the entity according to the mode}
    Procedure   Scale(x,y,z,rx,ry,rz:single);  {controls the relative speed between the mouse and the object}
    Procedure   Block(num:integer;valor:boolean);  {blocks movement and/or rotation on any of 6 axis.}
    Procedure   FindVertex(x,y:integer;Scene:TsceneGL;var pt:Tvertex); {return a pointer to the vertex which was nearer to the mouse}
                {Scene is the scene to be evaluated, pt is the chosen vertex, can be nil If none was found under the mouse}
                {x,y are the coordinates of the mouse}
  end;                   {num goes from 1 to 6,  valor=true:block, valor=false: don´t block movement}

Const
  MaxHits=200;
var
  xxx,yyy,nnn:integer;
  numFound:integer;
  VertexHits:Array[1..MaxHits] of integer;

{This function can convert a BMP file into a RGBA file}
{parameters: BMPname: name of the BMP file, RGBAname: name of the new file,
             transparent: color that will be treated as transparent}
Function ConvertBMP(BMPname:string;RGBAname:string;Transparent:longint):integer;

{this is to avoid certain problems with borland tools.  don´t call them yourself}
Function  MaskExceptions: Pointer;
Procedure UnmaskExceptions(OldMask: Pointer);

implementation

Constructor Tcamera.create;              {creates a new camera}
begin
  inherited create;
  LookAt(0,0,-100);   {the camera is looking to the bottom of the screen}
  SetVectorUp(0,1,0);  {the camera is rotated normally}
  SetPosition(0,0,8);  {the camera is in the center of the space}
end;

Procedure   Tcamera.Redraw;                {redraw camera}
begin
  GluLookAt(Position[1], Position[2], Position[3],
            SceneCenter[1], SceneCenter[2], SceneCenter[3],
            UpVector[1], UpVector[2], UpVector[3]);
end;

Procedure   Tcamera.SetPosition;
begin
  Position[1]:=ix;
  Position[2]:=iy;
  Position[3]:=iz;
end;

Procedure   Tcamera.LookAt;
begin
  SceneCenter[1]:=ix;
  SceneCenter[2]:=iy;
  SceneCenter[3]:=iz;
end;

Procedure   Tcamera.SetVectorUp;
begin
  UpVector[1]:=ix;
  UpVector[2]:=iy;
  UpVector[3]:=iz;
end;

Constructor TsceneGL.create;
begin
  inherited create;
  Entities:=Tlist.create;
  Lights:=Tlist.create;
  Cameras:=Tlist.create;
  DefaultCamera:=Tcamera.Create;
  ActiveCamera:=DefaultCamera;
  Cameras.Add(DefaultCamera);        {add the default camera to the cameras in the scene}
  Active:=false;
  Fperspective:=true;
  fogEnabled:=false;
  Angle:=30;  {degrees for Field-of-view angle}
  DistNear:=1; {1 unit of distance to near clipping plane}
  DistFar:=100; {distance to far clipping plane}
  texturing:=true;        {use textures}
  bTranslucentMode := false; // don't ignore depth buffer
end;

Destructor  TsceneGL.destroy;
Var I: Integer;
begin
  If Active then
    ReleaseRC;

  For I := 0 To Cameras.Count  - 1 Do TObject(Cameras.Items[I]).Free;
  For I := 0 To Lights.Count   - 1 Do TObject(Lights.Items[I]).Free;
  For I := 0 To Entities.Count - 1 Do TObject(Entities.Items[I]).Free;

  cameras.free;
  Lights.free;
  Entities.free;
  inherited destroy;
end;

Procedure TsceneGL.InitRC;
begin
  {set the area where the OpenGL rendering will take place}
  WindowHandle:=iHandle;
  DC := GetDC(WindowHandle);
  SetDCPixelFormat;
  HRC := wglCreateContext(DC);
  wglMakeCurrent(DC, HRC);
  { enable depht testing and back face rendering}
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
  glEnable(GL_LIGHTING);    {enable Lights}

  Active:=True;
end;

Procedure TsceneGL.SetDCPixelFormat;
var
  nPixelFormat: Integer;
  pfd: TPixelFormatDescriptor;
begin
  FillChar(pfd, SizeOf(pfd),0);
  with pfd do begin
    nSize     := sizeof(pfd);                               // Size of this structure
    nVersion  := 1;                                         // Version number
    dwFlags   := PFD_DRAW_TO_WINDOW or
                 PFD_SUPPORT_OPENGL or
                 PFD_DOUBLEBUFFER;                          // Flags
    iPixelType:= PFD_TYPE_RGBA;                             // RGBA pixel values
    cColorBits:= 24;                                        // 24-bit color
    cDepthBits:= 32;                                        // 32-bit depth buffer
    iLayerType:= PFD_MAIN_PLANE;                            // Layer type
  end;
  nPixelFormat := ChoosePixelFormat(DC, @pfd);
  SetPixelFormat(DC, nPixelFormat, @pfd);
  DescribePixelFormat(DC, nPixelFormat, sizeof(TPixelFormatDescriptor), pfd);
end;

Procedure TsceneGL.ReleaseRC;
begin
  wglMakeCurrent(0, 0);
  wglDeleteContext(HRC);
  ReleaseDC(WindowHandle, DC);
  Active:=False;
end;

Procedure TsceneGL.UpdateArea;
var
  gldAspect : GLdouble;
  ratio,range:GlFloat;
begin
  { redefine the visible volume and the viewport when the window´s size is modIfied}
  GlMatrixMode(GL_PROJECTION);
  GlLoadIdentity;
  If Fperspective then
  begin
    gldAspect := width/height;
    gluPerspective(Angle,           // Field-of-view angle
                 gldAspect,      // Aspect ratio of viewing volume
                 DistNear,            // Distance to near clipping plane
                 DistFar);          // Distance to far clipping plane
  end
    else
  begin { Orthogonal projection}
    range:=12;
    If width<=height then
    begin
      ratio:=height/width;
      GlOrtho(-range,range,-range*ratio,range*ratio,-range*4,range*4);
    end
      else
    begin
      ratio:=width/height;
      GlOrtho(-range*ratio,range*ratio,-range,range,-range*4,range*4);
    end;
  end;
  glViewport(0, 0, width, height);
end;

Procedure TsceneGL.Redraw;
const
  glfMaterialColor: Array[0..3] of GLfloat = (0.5, 1.0, 0.5, 1.0);
  ambientLight: Array[0..3] of GLfloat = (0.0, 0.0, 0.0, 0.0);
var
  i,num:integer;
  ps : TPaintStruct;
  pp:Pointer;
  bDepthTest : boolean;
begin
  If Active then
  begin
    {initialization}
    pp:=MaskExceptions;
    BeginPaint(WindowHandle, ps);
    GlMatrixMode(GL_MODELVIEW);  {reset translation and rotation values}
    GlLightModelfv(GL_light_model_ambient, @ambientLight);
    GlLoadIdentity;
      {Set Camera viewPoint}
    ActiveCamera.Redraw;
      {place Lights}
    i:=0;
    num:=Lights.count;
    while i<num do
    begin
      Tlight(Lights.items[i]).Redraw;
      inc(i);
    end;
    {clear depht and color buffers}
    glclearcolor(backR,backG,backB,1);  {specify background color}
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); {Clear the rendering area}
    {if translucent mode - ignore depth buffer}
    glGetBooleanv(GL_DEPTH_TEST,@bDepthTest);
    if (bTranslucentMode) then
     begin
        if (bDepthTest) then
         glDisable(GL_DEPTH_TEST);
        glBlendFunc(GL_SRC_ALPHA,GL_ONE);
        glEnable(GL_BLEND);
//        glDepthMask (GL_FALSE);
     end
    else
     begin
        if (not bDepthTest) then
         glEnable(GL_DEPTH_TEST);
        glDisable(GL_BLEND);
//        glDepthMask (GL_TRUE);
     end;

//    glEnable(GL_ALPHA_TEST);
//    glEnable(GL_BLEND);
//    glEnable(GL_RGBA_MODE);
//    glBlendFunc(GL_SRC_ALPHA,GL_ONE);

    glenable(gl_color_material);  {tell OpenGL to pay attention to GlColor orders}
    if fogEnabled then
    begin
      glenable(GL_fog);
      {too slow: glhint(gl_fog_hint, gl_nicest);}  {good fog, but a little slow}
      glfogi(gl_fog_mode, fogtype);
      glfogfv(gl_fog_color, @fogColor);
      if fogtype <> GL_linear then {density doesnt work with linear fog}
        glfogf(gl_fog_density, FogDensity);
      glfogf(GL_fog_start, fogMinDist);
      glfogf(GL_fog_end, fogMaxDist);
    end else
      gldisable(GL_fog);
    if texturing then
    begin
      gldisable(GL_texture_1d);
      glenable(gl_texture_2d);
    end
      else
    begin
      glDisable(GL_texture_1d);
      glDisable(gl_texture_2d);
    end;
    If PutNames then
    begin
      glInitNames;      {init the name stack, not necessary If your objects aren´t named}
      glPushName(0);    {init the name stack}
    end;
    {draw entities}
    i:=0;
    num:=Entities.count;
    while i<num do
    begin
       Tentity(Entities.items[i]).Trans := bTranslucentMode;
       Tentity(Entities.items[i]).Redraw;
       inc(i);
    end;
    {finalization}
    {swap the rendering buffers so there is no flickering}
    SwapBuffers(DC);
    EndPaint(WindowHandle, ps);
    UnmaskExceptions(pp);
  end;
end;

Procedure   TsceneGL.SetPerspective;
begin
  Angle:=iAngle;
  DistNear:=iDistNear;
  DistFar:=iDistFar;
end;

Procedure   TsceneGL.UseCamera;
begin
  if CameraNumber <= Cameras.Count then
    ActiveCamera:=Cameras.items[CameraNumber-1];
end;

Constructor Tlight.Create;    {by default a light is created white }
begin
  inherited create;
  Source:=Tvertex.create(nil,0,0,0);  {light with no orientation...}
  Source.Point:=Tpoint.Create(0,0,0);  {...and placed in the center of the scene}
  Ambient(0.5,0.5,0.5,1);
  DIffuse(0.25,0.25,0.25,1);
  Specular(0.1,0.1,0.1,1);
  Enabled:=true;          {by default the light is enabled}
  LightType:=CLambiental;   {by default the light is ambiental}
  CutOffAngle:=60;    {but the default cut-off angle is 60 degrees}
  SpotExponent:=100;  {and the spot exponent is very shiny}
  attenuation:=0.01;   {attenuation default value}
  attenuationType:=CLlinear;  {attenuation type}
  case num of
    1: Number:=GL_Light0;
    2: Number:=GL_Light1;
    3: Number:=GL_Light2;
    4: Number:=GL_Light3;
    5: Number:=GL_Light4;
    6: Number:=GL_Light5;
    7: Number:=GL_Light6;
    8: Number:=GL_Light7;
  end; {case}
end;

Destructor  Tlight.destroy;
begin
  Source.point.free;
  Source.free;
end;

Procedure   Tlight.Ambient;
begin
  Fambient[0]:=r;
  Fambient[1]:=g;
  Fambient[2]:=b;
  Fambient[3]:=a;
end;

Procedure   Tlight.Diffuse;
begin
  fDIffuse[0]:=r;
  fDIffuse[1]:=g;
  fDIffuse[2]:=b;
  fDIffuse[3]:=a;
end;

Procedure   Tlight.Specular;
begin
  Fspecular[0]:=r;
  Fspecular[1]:=g;
  Fspecular[2]:=b;
  Fspecular[3]:=a;
end;

Procedure   Tlight.SetOrientation;
begin
  Source.nx:=nx;
  Source.ny:=ny;
  Source.nz:=nz;
end;

Procedure   Tlight.Redraw;
var
  Fposition:Array[0..3] of GLfloat;
  Frotation:Array[0..3] of GLfloat;
begin
  if Enabled then
  begin
  {  GlPushMatrix(); }
    if (LightType <> CLambiental) then
    begin
      {the light has a location}
      Fposition[0]:=Source.Point.x;
      Fposition[1]:=Source.Point.y;
      Fposition[2]:=Source.Point.z;
      Fposition[3]:=1.0;  {indicates that the light is actually present at that location}
                          {if it were zero, the rays would be parallel like a sun}
      Frotation[0]:=Source.nx;
      Frotation[1]:=Source.ny;
      Frotation[2]:=Source.nz;
      Frotation[3]:=1.0;  {don´t know what this indicates}
      if (LightType = CLSpot) then
      begin
        glLightF(Number, GL_Spot_Cutoff, CutOffAngle);
        glLightF(Number, GL_Spot_Exponent, SpotExponent);
        glLightfv(number, GL_Spot_Direction, @Frotation);
      end;
      glLightfv(number, GL_Position, @Fposition);
    end;
    GlLightfv(Number, GL_AMBIENT, @Fambient);
    GlLightfv(Number, GL_DIFFUSE, @Fdiffuse);
    GlLightfv(Number, GL_SPECULAR,@Fspecular);
    case attenuationType of
           CLconstant:GlLightf(number, GL_Constant_attenuation, attenuation);
             CLlinear:GlLightf(number, GL_Linear_attenuation, attenuation);
          CLquadratic:GlLightf(number, GL_Quadratic_attenuation, attenuation);
        end; {case}
    GlEnable(Number);   {enable light number N}
  {  GlPopMatrix();}
  end;
end;

Constructor T3dMouse.Create;
var
  i:integer;
begin
  inherited create;
  Entity:=iEntity;
  Mode:=3;  {this is the mode used by defect, just because it fits my needs}
  Button1:=false;
  Button2:=false;
  for i:=1 to 6 do
    BlockStatus[i]:=false;
end;

Procedure   T3dMouse.Move;
begin
  If assigned(Entity) then
  begin
    If not Button1 then
    begin
      If ssLeft in botones then
      begin
        If mode=1 then  {X,Y,Z}
        begin
          Start[1]:=x-Entity.Position[1]/Scaling[1];     {X}
          Start[2]:=y-Entity.Position[2]/Scaling[2];     {Y}
        end;
        If mode in [2,3] then  {2:rx,ry,rz  3:rx,ry,rz,Z}
        begin
          Start[6]:=x-Entity.rotation[3]/Scaling[6];  {rx}
          Start[4]:=y-Entity.rotation[1]/Scaling[4];  {ry}
        end;
        Button1:=true;
      end;
    end
      else
    begin
      If ssLeft in botones then
      begin
        If mode=1 then
        begin
          If not BlockStatus[1] then
            Entity.Position[1]:=(x-Start[1])* Scaling[1];     {X}
          If not BlockStatus[2] then
            Entity.Position[2]:=(y-Start[2])* Scaling[2];     {Y}
        end;
        If mode in [2,3] then
        begin
          If not BlockStatus[4] then
            Entity.rotation[3]:=(x-Start[6])* Scaling[6];  {rx}
          If not BlockStatus[5] then
            Entity.rotation[1]:=(y-Start[4])* Scaling[4];  {ry}
        end;
      end
        else
      Button1:=false;
    end;
    If not Button2 then
    begin
      If ssRight in botones then
      begin
        If mode in [1,3] then
          Start[3]:=y-Entity.Position[3]/Scaling[3];     {z}
        If mode in [2,3] then
          Start[5]:=x-Entity.rotation[2]/Scaling[5];     {rz}
        Button2:=true;
      end;
    end
      else
    begin
      If ssRight in botones then
      begin
        If mode in [1,3] then
          If not BlockStatus[3] then
            Entity.Position[3]:=(y-Start[3])* Scaling[3];     {Z}
        If mode in [2,3] then
          If not BlockStatus[6] then
            Entity.rotation[2]:=(x-Start[5])* Scaling[5];  {rz}
      end
        else
      Button2:=false;
    end;
  end;
end;

Procedure   T3dMouse.Scale;
begin
  Scaling[1]:=x;
  Scaling[2]:=y;
  Scaling[3]:=z;
  Scaling[4]:=rx;
  Scaling[5]:=ry;
  Scaling[6]:=rz;
end;

Procedure T3dMouse.Block;
begin
  If num in [1..6] then
    BlockStatus[num]:=valor;
end;

Procedure   T3dMouse.FindVertex;
const
  tambuffer=8000;  {8000 items reserved for the rendering buffer}
  radio=15;         {this is the search radius}
var
  buffer:array[0..tamBuffer] of GLfloat;
  size,i,j,count:integer;
  nx,ny:integer;
  PreviousPutNames:boolean;
  ActualVertex:LongInt;

begin
  PreviousPutNames:=PutNames;  {preserve the previous value of PutNames}
  PutNames:=true;  {put names on each vertex}
  numFound:=0;
  GlFeedBackBuffer(tamBuffer,GL_2D,@buffer);
  GlRenderMode(GL_feedBack);
  Scene.Redraw;
  size:=GlRenderMode(GL_render);
  {now that we got the 2D coordinates of each vertex, find which vertex is near}
  i:=0;
  try
  while i<size do
  begin
    If buffer[i]=GL_Pass_Through_token then   {the G.P.T.T. is a marker to divide the buffer in sections}
      If buffer[i+1]=Entity.id then    {this is the entity we are dealing with}
      begin
        inc(i,2);
        // continue cicling until we find another Object marker:
        while (buffer[i]=GL_Pass_Through_token) do
        begin
          ActualVertex:=round(Buffer[i+1]);
          If buffer[i+2]=GL_Polygon_Token then    {this is a polygon, let´s check it out}
          begin
            count:=trunc(buffer[i+3]);
            inc(i,4);
            for j:=0 to count-1 do  {let´s take a look at each vertex of this polygon}
            begin
              nx:=round(buffer[i]);    {x coordinate of a vertex}
              ny:=round(buffer[i+1]);    {y coordinate of a vertex}
              If (nx+radio>x) and (nx-radio<x)
              and (ny+radio>y) and (ny-radio<y) and (NumFound<MaxHits) then
              begin
                inc(numFound);
                VertexHits[numFound]:=ActualVertex+j;
              end;
              inc(i,2); {x and y}
            end;
          end
            else
              inc(i,2);
        end;
      end;
    inc(i);
  end;
  except
  end;
  PutNames:=PreviousPutNames;  {restore the previous value of PutNames}
end;

{MWMWMWMWMWMWMWMWMW   END OF CLASS IMPLEMENTATION WMWMWMWMMWMWMWMWM}
Function MaskExceptions: Pointer;
var
  OldMask : Pointer;
begin
  asm
    fnstcw WORD PTR OldMask;
    mov eax, OldMask;
    or eax, $3f;
    mov WORD PTR OldMask+2,ax;
    fldcw WORD PTR OldMask+2;
  end;
  result := OldMask;
end;

Procedure UnmaskExceptions(OldMask: Pointer);
begin
  asm
    fnclex;
    fldcw WORD PTR OldMask;
  end;
end;

Function ConvertBMP(BMPname:string;RGBAname:string;Transparent:longint):integer;
begin
  {this will convert a RGB bmp to a RGBA bmp, it is not ready yet}
  result:=0;
end;

end.
