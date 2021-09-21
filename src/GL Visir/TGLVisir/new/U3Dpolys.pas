{HACER QUE TODO TPOINT APUNTE SIEMPRE A SUS VERTICES Y QUE DEJE DE HACERLO SI
 UN VERTICE SE DESPRENDE}
 {ELIMINAR CAMPO REFERENCES DE TPOINT}

{3D object handling.  this unit is related to uRickGL

  Tentity:  an entity or object
      tface: a face of an entity, each entity has many faces
         Tvertex: a vertex of a face, each face has many vertices.

   By: Ricardo Sarmiento
       delphiman@hotmail.com
 }
unit U3Dpolys;

interface

Uses Windows, classes, SysUtils, gl,glu, Dialogs;

{***************     special record definition    *******************}
type
  GlyphMetricsFloat=record  {this one is used in the 3D text entity}
    gmfblackBoxX:float;
    gmfblackboxY:float;
    gmfptglyphorigin:pointer;
    gmfcellincX:float;
    gmfcellincY:float;
  end;

{MWMWMWMWMWMWMWMWMWMW    Classes definition  MWMWMWMWMWMWMWMWMWMWMWMW}
type
  tface=Class;
  Tpoint = Class;
  Ttexture = Class;

Tentity = Class(Tobject)    {entity or object or 3D mesh}
    trans : Boolean;
    
    Faces:TList;                       {2D polygons list belonging to the mesh}
    Points:Tlist;                      {list of all the vertices of the entity}
    R,G,B,A:Byte;                      {default color to use }
    R1,G1,B1,A1:Byte;                  {colors pushed}
    BoundR,BoundG,BoundB : Byte;       {bound lines color}
    Position:array[1..3] of single;    {position of entity in space   X,Y,Z}
    localPosition:array[1..3] of single;{current delta from position of entity in space X,Y,Z}
    rotation:array[1..3] of single;    {Orientation, Rx,Ry,Rz}
    scale:array[1..3] of single;       {Zoom x,y,z}
    id:GLuInt;                         {entity's identIfier, optional}
    Texture:Ttexture;                  {pointer to a texture}
    wireframe:byte;                    {0: use points, 1:use wireframes, 2: use solid poligons}
    textured:boolean;                  {indicates if the texture has been created or not}
    visible:boolean;                   {indicates if the object can be seen or not}
  constructor Create;                {create a zero-polygon entity}
  Destructor  Destroy; override;
  Procedure   Load(st:String);  {load from propietary file format}
  Procedure   Save(st:String);  {not yet implemented}
  Procedure   LoadFromStream(st:TFileStream);  {load from file stream}
  Procedure   SaveToStream(st:TFileStream);
  Procedure   SetColor(iR,iG,iB,iA:Byte);    {change entity�s color}
  Procedure   PushColors; {push used colors}
  Procedure   PopColors;  {pop them}
  Procedure   SetBoundColor(iR,iG,iB:Byte);    {change entity's bounds color}
  Procedure   Move(X,Y,Z:single);     {move entity to new position}
  Procedure   Rotate(Rx,Ry,Rz:single);  {turn entity to new orientation}
  Procedure   Redraw;  virtual;       {construct the entity using OpenGL commands}
  Procedure   LoadDXF(st:String;culling:boolean);  {Read DXF file in ST and generate the mesh}
              {if param culling is true, generate two opposite faces for each polygon in the DXF file}
  Procedure   CalcNormals;         {calculate normals for each vertex}
  Procedure   MoveVertices(dx,dy,dz:double);{add dx,dy,dz to all points}
  Procedure   MulVertices(dx,dy,dz:double);{mul to dx,dy,dz all points}
  Procedure   GetDiapasones(var x1,x2,y1,y2,z1,z2:double);{find the diapasones of x,y,z values}
  Procedure   Center;                 {find the geometric center of the entity and put it at 0,0,0}
  Function    AddFace:tface;       {add a new, empty face to the entity}
  Function    FindPoint(ix,iy,iz:single):Tpoint;    {search for a point near ix,iy,iz}
  Function    CreateTexture:TTexture;       {create and assign a texture to the entity}
  Procedure   DeleteTexture;                {delete the texture}
end; {Tentity}                        {useful when rotating entity around it�s center}

T3Dtext = class(Tentity)  {a special kind of entity}
    localRotation:array[1..3] of single;    {Orientation about own center, Rx,Ry,Rz}
    fontScale:array[1..3] of single;       {Scale of font in x,y,z}
    FontStyle:TlogfontA;   {the font settings}
    ExtrudeDepth:float;    {the Z-axis depth when creating the 3D font}
    ListStart:integer;     {indicates where to have the display lists, two fonts cannot use the same space
                            each font uses 256 spaces}
    {call ResetFont after changing one or more of the above properties to apply the changes}
    Text:Array[0..255] of char;           {the message to be written}
    TextLength:integer;
    myDC:HDC;             {the Device Context of the TsceneGL}
  constructor Create(DC:HDC; iListStart:integer);overload;                {create a 3D text}
  constructor Create(otherText:T3Dtext);overload;                {create a 3D text}
  Destructor  Destroy; override;
  Procedure   ResetFont;  {recreate the 3D font, useful after changing the properties}
  Procedure   Redraw; override;     {redraw the 3D text}
  Procedure   SetText(itext:string);  {change the text property}
  Procedure   localRotate(Rx,Ry,Rz:single);  {turn entity to new orientation}
  {the following methods are not available for 3Dtext.
       Procedure   Load
       Procedure   Save
       Procedure   LoadDXF
       Procedure   CalcNormals;   (is not necessary)
       Procedure   Center;        (could be done, but I don�t have the patience)
       Function    AddFace:tface;
       Function    FindPoint      (find a point in the display lists ?, I need a volunteer, or a hero)
  }
end;  {T3Dtext}

tface=Class(Tobject)              {a face or polygon of an entity}
  Vertices:Tlist;                      {the mesh�s vertex list}
  r,g,b,a:Byte;                        {face color}
  r1,g1,b1,a1:Byte;                    {pushed face color}
  Owner:Tentity;                    {points to the owner entity}
  ApplyTexture:boolean;             {use texturing or not in this face}
  constructor Create(iOwner:Tentity);  {create the face and assign its owner entity}
  Destructor  Destroy; override;
  Procedure   AddVertex(x,y,z,nx,ny,nz:single);  {add vertex at X,Y,Z with normal nx,ny,nz}
  Procedure   Redraw;                                {draw face using OpenGL commands}

  {calculate normal given 3 points of the face, normally not called directly}
  Procedure   CalcNormal(uno,dos,tres:integer;var inx,iny,inz:single);
  Procedure   SetColor(ir,ig,ib,ia:Byte);  {set the desired color of the face}
  Procedure   PushColor;  {push color of the face}
  Procedure   PopColor;  {pop color of the face}
end;  {tface}

Tpoint = Class(Tobject)       {a colored point in space}
  x,y,z:single;             {position x,y,z}
  r,g,b,a:Byte;               {vertex�s color, rgb}
  r1,g1,b1,a1:Byte;           {pushed vertex�s color, rgb}
  References:Byte;          {number of vertices using the point for color and position}
  Vertices:Tlist;
  Constructor Create(ix,iy,iz:single);       {set position and References to 0}
  Destructor  Destroy;  override;
  Procedure   SetColor(ir,ig,ib:Byte);  {set the desired color of the point}
  Procedure   PushColor;  {push color of the point}
  Procedure   PopColor;  {pop color of the point}
  Procedure   SetPosition(ix,iy,iz:single);  {move the point to a dIfferent place}
end;  {Tpoint}

Tvertex = Class(Tobject)   {a vertex of a flat polygon}
  nx,ny,nz:single;         {normal vector, each vertex has it�s own normal vector, this is useful for certain tricks}
  point:Tpoint;            {points to position and color data}
  Owner:Tface;             {points to the face that owns the vertex}
  Tx,Tz:single;            {Texture X and Z coordinates}
  constructor Create(iowner:Tface;inx,iny,inz:single);    {create vertex with its normal}
  Procedure   SetColor(ir,ig,ib:Byte);  {set the desired individual color of the vertex}
  Procedure   SetPosition(ix,iy,iz:single);  {move individually the vertex to a dIfferent place}
  Procedure   MakeCopy;   {create a new Tpoint instance, so that the vertex can modIfy individualy the color and position}
end; {Tvertex}

Ttexture = Class(Tobject) {a texture}
  Automatic:boolean;      {use or not automatic texture coordinates generation}
  AutoXmult:array[0..3] of GLint; {multiply values for X coordinate}
  AutoZmult:array[0..3] of GLint; {multiply values for Z coordinate}
  AutoGenModeX:GLint;  {coordinate calculation algorithm to be used: }
  AutoGenModeZ:GLint;  {GL_object_linear, GL_Eye_linear or GL_Sphere_map}
  WrapSMode:Glint;
  WrapTMode:Glint;
  MagFilter:Glint;
  MinFilter:Glint;
  EnvironmentMode:GLint;  {GL_decal, GL_modulate or GL_blend}
  EnvBlendColor:Array[0..3] of byte;  {RGBA color if EnvironmentMode is blend}
  Owner:Tentity;       {a texture can be owned by an Entity}
  MyList:GLsizei;      {number of the display list for this texture}
  Constructor Create(iowner:Tentity);  {set defaults for all fields}
  Destructor  destroy; override;{destroy the display list}
  Function    LoadTexture(st:string):shortint;  {load a new texture file, return 0 if ok, negative number if error}
  Procedure   Redraw;   {call the display list, it�s not really a redraw, it�s part of one}
end;  {Ttexture}

Const   {global constants}
  MinimalDistance=0.0001;   {If two points are this far from each other, they can be considered to be in the same position}

var  {global variables}
       //  this two variables are used in this unit and in unit UrickGL:
  PutNames:boolean;  {If true put names to vertex and entity primitives when rendering}
  ActualVertexNumber:LongInt;  {used to mark every vertex with a dIfferent name in every entity}
implementation
{MWMWMWMWMWMWMWMWMWMW  IMPLEMENTATION of the CLASSES  MWMWMWMWMWMWMWMWMWMWMWMW}




Uses UrickGL;




constructor Tentity.create;
begin
  inherited create;
  Trans := False;
  id:=0;                {initiallly this value has no importance}
  Faces:=Tlist.create;
  Points:=Tlist.create;
  SetColor(128,128,128,255); {use a medium grey color}
  A:=255;  {alpha component is by default opaque}
  wireframe:=2;      {by default use solid poligons for rendering}
  textured:=False;   {a texture has not been assigned to this entity}
  Visible:=true;     {by default the entity is visible}
  scale[1] := 1;
  scale[2] := 1;
  scale[3] := 1;
  BoundR := 255; BoundG := 255; BoundB := 255;
end;

Destructor  Tentity.destroy;
Var I: Integer;
begin
  if textured then
    DeleteTexture;  {erase the texture if it exists}
  For I := 0 To Points.Count  - 1 Do TObject(Points.Items[I]).Free;
  For I := 0 To Faces.Count   - 1 Do TObject(Faces.Items[I]).Free;
  Points.Free;
  Faces.Free;
  inherited Destroy;
end;

Procedure   Tentity.Load;
var
  f:file;
  numFaces,
  NumPoints,
  NumTextures,
  i,j:LongInt;
  IdData:array[0..3] of char;
  Reserved:array[1..20] of Byte;
  ix,iy,iz:single;
  inx,iny,inz:single;
  ir,ig,ib:Byte;
  Point:Tpoint;
  Face:Tface;
  Vertex:Tvertex;
  PointNum:longint;
  numVertices:byte;   {this limits the vertex count for each polygon to less than 255 vertices, more than enough}
  Version,
  SubVersion:byte;
begin
  assignFile(f,st);
  If not FileExists(st) then
    exit;
  Reset(f,1);
  BlockRead(f,IdData,sizeof(IdData));
  BlockRead(f,Version,sizeof(version));
  BlockRead(f,SubVersion,sizeof(SubVersion));
  if version=1 then
  begin
    {first clear old data stored in object}
    Faces.Clear;
    Points.Clear;
    {then, begin to read new data}
    BlockRead(f,ir,sizeof(ir));
    BlockRead(f,ig,sizeof(ig));
    BlockRead(f,ib,sizeof(ib));
    SetColor(ir,ig,ib,255);
    BlockRead(f,numFaces,sizeof(numFaces));
    BlockRead(f,numPoints,sizeof(numPoints));
    BlockRead(f,numTextures,sizeof(numTextures));  {not used yet}
    BlockRead(f,Reserved,sizeof(Reserved));        {for future purposes}
    {read Points}
    i:=0;
    while i<NumPoints do
    begin
      BlockRead(f,ix,sizeof(ix));
      BlockRead(f,iy,sizeof(iy));
      BlockRead(f,iz,sizeof(iz));
      BlockRead(f,ir,sizeof(ir));
      BlockRead(f,ig,sizeof(ig));
      BlockRead(f,ib,sizeof(ib));
      Point:=Tpoint.create(ix,iy,iz);
      Point.SetColor(ir,ig,ib);
      Points.add(point);
      inc(i);
    end;
    {Read faces}
    i:=0;
    while i<NumFaces do
    begin
      BlockRead(f,ir,sizeof(ir));
      BlockRead(f,ig,sizeof(ig));
      BlockRead(f,ib,sizeof(ib));
      BlockRead(f,numVertices,SizeOf(numVertices));
      Face:=AddFace;
      Face.SetColor(ir,ig,ib,255);
      j:=0;
      While j<NumVertices do
      begin
        BlockRead(f,inx,sizeof(inx));
        BlockRead(f,iny,sizeof(iny));
        BlockRead(f,inz,sizeof(inz));
        BlockRead(f,PointNum,sizeof(PointNum));
        Vertex:=Tvertex.create(Face,inx,iny,inz);
        Vertex.Point:=Tpoint(Points.items[PointNum]);
        Vertex.Point.Vertices.add(Vertex);  {the point must have the references to its vertices}
        inc(Vertex.Point.references);
        Face.Vertices.add(vertex);
        inc(j);
      end;
      inc(i);
    end;
    {Read Texture coordinates, not yet implemented}
  end;
  CloseFile(f);
end;

Procedure   Tentity.Save;
var
  f:file;
  numFaces,
  NumPoints,
  NumTextures,
  i,j:LongInt;
  IdData:array[0..3] of char;
  Reserved:array[1..20] of Byte;
  Point:Tpoint;
  Face:Tface;
  Vertex:Tvertex;
  PointNum:longint;
  numVertices:byte;   {this limits the vertex count for each polygon to less than 255 vertices, more than enough}
  Version,
  SubVersion:byte;
begin
  assignFile(f,st);
  ReWrite(f,1);
  IdData[0]:='3';  IdData[1]:='D';  IdData[2]:='P';  IdData[3]:='F';  {3DPF: 3D Propietary Format}
  Version:=1;    {this file was stored using algorithm version 1. }
  SubVersion:=0; {this file was stored using algorithm version  .0}
  NumFaces:=Faces.count;
  NumPoints:=Points.Count;
  NumTextures:=0;         {by now no textures are allowed}
  BlockWrite(f,IdData,sizeof(IdData));
  BlockWrite(f,Version,sizeof(Version));
  BlockWrite(f,SubVersion,sizeof(SubVersion));
  BlockWrite(f,r,sizeof(r));
  BlockWrite(f,g,sizeof(g));
  BlockWrite(f,b,sizeof(b));
  BlockWrite(f,NumFaces,sizeof(NumFaces));
  BlockWrite(f,NumPoints,sizeof(NumPoints));
  BlockWrite(f,NumTextures,sizeof(NumTextures));  {not used yet}
  BlockWrite(f,Reserved,sizeof(Reserved));        {for future purposes}
  {Write Points}
  i:=0;
  while i<NumPoints do
  begin
    Point:=Tpoint(Points.items[i]);
    with point do
    begin
      BlockWrite(f,x,sizeof(x));
      BlockWrite(f,y,sizeof(y));
      BlockWrite(f,z,sizeof(z));
      BlockWrite(f,r,sizeof(r));
      BlockWrite(f,g,sizeof(g));
      BlockWrite(f,b,sizeof(b));
    end;
    inc(i);
  end;
  {Write faces}
  i:=0;
  while i<NumFaces do
  begin
    Face:=Tface(Faces.items[i]);
    with face do
    begin
      NumVertices:=Vertices.count;
      BlockWrite(f,r,sizeof(r));
      BlockWrite(f,g,sizeof(g));
      BlockWrite(f,b,sizeof(b));
      BlockWrite(f,NumVertices,SizeOf(NumVertices));
    end;
    j:=0;
    While j<NumVertices do
    begin
      Vertex:=Tvertex(Face.vertices.items[j]);
      with Vertex do
      begin
        PointNum:=Points.Indexof(Point);
        BlockWrite(f,nx,sizeof(nx));
        BlockWrite(f,ny,sizeof(ny));
        BlockWrite(f,nz,sizeof(nz));
        BlockWrite(f,PointNum,sizeof(PointNum));
      end;
      inc(j);
    end;
    inc(i);
  end;
  {Write Texture coordinates, not yet implemented}
  CloseFile(f);
end;


Procedure   Tentity.SaveToStream;
var
  numFaces,
  NumPoints,
  i,j:LongInt;
  Point:Tpoint;
  Face:Tface;
  Vertex:Tvertex;
  PointNum:longint;
  numVertices:byte;   {this limits the vertex count for each polygon to less than 255 vertices, more than enough}
begin
 // save entity header
  NumFaces:=Faces.count;
  NumPoints:=Points.Count;
  st.Write(numPoints,sizeof(numPoints));
  st.Write(numFaces,sizeof(numPoints));

  st.Write(Position,sizeof(Position[1])*3);
  st.Write(localPosition,sizeof(localPosition[1])*3);
  st.Write(Rotation,sizeof(Rotation[1])*3);
  st.Write(Scale,sizeof(Scale[1])*3);
  st.Write(r,sizeof(r));
  st.Write(g,sizeof(g));
  st.Write(b,sizeof(b));
  st.Write(BoundR,sizeof(BoundR));
  st.Write(BoundG,sizeof(BoundG));
  st.Write(BoundB,sizeof(BoundB));
  st.Write(wireframe,sizeof(wireframe));
  st.Write(visible,sizeof(visible));

  {Write Points}
  i:=0;
  while i<NumPoints do
  begin
    Point:=Tpoint(Points.items[i]);
    with point do
    begin
      st.Write(x,sizeof(x));
      st.Write(y,sizeof(y));
      st.Write(z,sizeof(z));
      st.Write(r,sizeof(r));
      st.Write(g,sizeof(g));
      st.Write(b,sizeof(b));
    end;
    inc(i);
  end;
  {Write faces}
  i:=0;
  while i<NumFaces do
  begin
    Face:=Tface(Faces.items[i]);
    with face do
    begin
      NumVertices:=Vertices.count;
      st.Write(r,sizeof(r));
      st.Write(g,sizeof(g));
      st.Write(b,sizeof(b));
      st.Write(a,sizeof(a));
      st.Write(NumVertices,SizeOf(NumVertices));
    end;
    j:=0;
    While j<NumVertices do
    begin
      Vertex:=Tvertex(Face.vertices.items[j]);
      with Vertex do
      begin
        PointNum:=Points.Indexof(Point);
        st.Write(nx,sizeof(nx));
        st.Write(ny,sizeof(ny));
        st.Write(nz,sizeof(nz));
        st.Write(PointNum,sizeof(PointNum));
      end;
      inc(j);
    end;
    inc(i);
  end;
end;

Procedure   Tentity.LoadFromStream;
var
  numFaces,
  NumPoints,
  i,j:LongInt;
  ix,iy,iz:single;
  inx,iny,inz:single;
  ir,ig,ib,ia:Byte;
  Point:Tpoint;
  Face:Tface;
  Vertex:Tvertex;
  PointNum:longint;
  numVertices:byte;   {this limits the vertex count for each polygon to less than 255 vertices, more than enough}
begin
  Faces.Clear;
  Points.Clear;

 // save entity header
  NumFaces:=Faces.count;
  NumPoints:=Points.Count;
  st.Read(numPoints,sizeof(numPoints));
  st.Read(numFaces,sizeof(numPoints));

  st.Read(Position,sizeof(Position[1])*3);
  st.Read(localPosition,sizeof(localPosition[1])*3);
  st.Read(Rotation,sizeof(Rotation[1])*3);
  st.Read(Scale,sizeof(Scale[1])*3);
  st.Read(r,sizeof(r));
  st.Read(g,sizeof(g));
  st.Read(b,sizeof(b));
  st.Read(BoundR,sizeof(BoundR));
  st.Read(BoundG,sizeof(BoundG));
  st.Read(BoundB,sizeof(BoundB));
  st.Read(wireframe,sizeof(wireframe));
  st.Read(visible,sizeof(visible));

    {read Points}
    i:=0;
    while i<NumPoints do
    begin
      st.Read(ix,sizeof(ix));
      st.Read(iy,sizeof(iy));
      st.Read(iz,sizeof(iz));
      st.Read(ir,sizeof(ir));
      st.Read(ig,sizeof(ig));
      st.Read(ib,sizeof(ib));
      Point:=Tpoint.create(ix,iy,iz);
      Point.SetColor(ir,ig,ib);
      Points.add(point);
      inc(i);
    end;
    {Read faces}
    i:=0;
    while i<NumFaces do
    begin
      st.Read(ir,sizeof(ir));
      st.Read(ig,sizeof(ig));
      st.Read(ib,sizeof(ib));
      st.Read(ia,sizeof(ia));
      st.Read(numVertices,SizeOf(numVertices));
      Face:=AddFace;
      Face.SetColor(ir,ig,ib,ia);
      j:=0;
      While j<NumVertices do
      begin
        st.Read(inx,sizeof(inx));
        st.Read(iny,sizeof(iny));
        st.Read(inz,sizeof(inz));
        st.Read(PointNum,sizeof(PointNum));
        Vertex:=Tvertex.create(Face,inx,iny,inz);
        Vertex.Point:=Tpoint(Points.items[PointNum]);
        Vertex.Point.Vertices.add(Vertex);  {the point must have the references to its vertices}
        inc(Vertex.Point.references);
        Face.Vertices.add(vertex);
        inc(j);
      end;
      inc(i);
    end;
end;

Procedure   Tentity.SetColor;
var i:integer;
begin
  R:=iR;
  G:=iG;
  B:=iB;
  A:=iA;
  for i:=0 to Faces.Count-1 do
   TFace(Faces.Items[i]).SetColor(r,g,b,a);
  for i:=0 to Points.Count-1 do
   TPoint(Points.Items[i]).SetColor(r,g,b);
end;

Procedure   Tentity.PushColors;
var i:integer;
begin
  R1:=R;
  G1:=G;
  B1:=B;
  A1:=A;
  for i:=0 to Faces.Count-1 do
   TFace(Faces.Items[i]).PushColor;
  for i:=0 to Points.Count-1 do
   TPoint(Points.Items[i]).PushColor;
end;

Procedure   Tentity.PopColors;
var i:integer;
begin
  R:=R1;
  G:=G1;
  B:=B1;
  A:=A1;
  for i:=0 to Faces.Count-1 do
   TFace(Faces.Items[i]).PopColor;
  for i:=0 to Points.Count-1 do
   TPoint(Points.Items[i]).PopColor;
end;

Procedure   Tentity.SetBoundColor;
begin
  BoundR:=iR;
  BoundG:=iG;
  BoundB:=iB;
end;

Procedure   Tentity.Move;
begin
  Position[1]:=x;
  Position[2]:=y;
  Position[3]:=z;
end;

Procedure   Tentity.Rotate;
begin
  rotation[1]:=rx;
  rotation[2]:=ry;
  rotation[3]:=rz;
end;

Procedure   Tentity.Redraw;
var
  i,num,step:integer;
  r1,g1,b1 : integer;
begin
  if visible then
  begin
    step:=0;
    PushColors;
    GlPushMatrix();
    r1 := r; g1:=g; b1:=b;
    glScalef(scale[1],scale[2],scale[3]);
    glTranslatef(localPosition[1],localPosition[2],localPosition[3]);   {translate, not the object but the coordinate system}
    glRotatef(rotation[1], 1.0, 0.0, 0.0);   {same, rotate the coordinate system}
    glRotatef(rotation[2], 0.0, 1.0, 0.0);
    glRotatef(rotation[3], 0.0, 0.0, 1.0);
    glTranslatef(Position[1],Position[2],Position[3]);   {translate, not the object but the coordinate system}
    if Assigned(Texture) then  {there is a texture assigned}
      Texture.redraw;
    while (step < 2) do
    begin
    case wireframe of
      0:GlPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
      1:GlPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      2:GlPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      3:if (step=0) then
         GlPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        else
         begin
          SetColor(boundR,boundG,boundB,255);
          GlPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
          glLineWidth(2);
         end;
      end;


    If PutNames then  {If this is active then each entity has it�s own name}
    begin
      GlLoadName(id);
      GLPassThrough(id);   {it should be:  GLfloat(id), but it wouldn�t compile}
    end;

    i:=0;
    num:=Faces.count;
    while i<num do
    begin
      If PutNames then
      begin
        ActualVertexNumber:=i;  {from 0 to Faces.count-1}
        ActualVertexNumber:=ActualVertexNumber shl 16;  {a LongInt has 32 bits, so this shIfts the actual face number to the upper 16 bits}
      end;
      tface(Faces.items[i]).Redraw;
      inc(i);
    end;
    if (wireframe <> 3) then step := 2
    else step := step+1;
   end;
   r := r1; g:=g1; b:=b1;
   PopColors;
   glLineWidth(1);
   GlPopMatrix();
  end;
end;

Procedure Tentity.LoadDXF;
var
  f:textfile;
  st1,st2:string;
  in_entities:boolean;
  kind,group,err:integer;
  analyzing:boolean;
  x1,x2,y1,y2,z1,z2,x3,y3,z3,x4,y4,z4:single;
  Face:tface;
Procedure DivRectangle;
begin
  Face:=AddFace;
  Face.AddVertex(x4,y4,z4,0,0,1);
  Face.AddVertex(x3,y3,z3,0,0,1);
  Face.AddVertex(x2,y2,z2,0,0,1);
  Face:=AddFace;
  Face.AddVertex(x4,y4,z4,0,0,1);
  Face.AddVertex(x2,y2,z2,0,0,1);
  Face.AddVertex(x1,y1,z1,0,0,1);
  if Culling then
  begin {for 3D studio: double the faces but looking the opposite way}
    Face:=AddFace;
    Face.AddVertex(x2,y2,z2,0,0,1);
    Face.AddVertex(x3,y3,z3,0,0,1);
    Face.AddVertex(x4,y4,z4,0,0,1);
    Face:=AddFace;
    Face.AddVertex(x1,y1,z1,0,0,1);
    Face.AddVertex(x2,y2,z2,0,0,1);
    Face.AddVertex(x4,y4,z4,0,0,1);
  end;
end;
begin
  {first clear old data stored in object}
  Faces.Clear;
  Points.Clear;
  {then, begin}
  assignfile(f,st);
  reset(f);
  in_entities:=false;
  repeat
    readln(f,st1);
    If st1='ENTITIES' then in_entities:=true;
  until in_entities or eof(f);
  analyzing:=false;
  kind:=0;
  x1:=0;   x2:=0;   x3:=0;   x4:=0;
  y1:=0;   y2:=0;   y3:=0;   y4:=0;
  z1:=0;   z2:=0;   z3:=0;   z4:=0;
  If in_entities then
    repeat
      readln(f,st1);
      readln(f,st2);
      group:=StrToInt(st1);
      case group of
        0:begin
            If analyzing then
            begin
              case kind of
                1:begin
                    if (x4<>x3) or (y4<>y3) or (z4<>z3) then
                      DivRectangle
                    else
                      begin
                        Face:=AddFace;
                        Face.AddVertex(x3,y3,z3,0,0,1);
                        Face.AddVertex(x2,y2,z2,0,0,1);
                        Face.AddVertex(x1,y1,z1,0,0,1);
                        if Culling then
                        begin {for 3D studio: double the faces but
                               looking the opposite way}
                          Face:=AddFace;
                          Face.AddVertex(x1,y1,z1,0,0,1);
                          Face.AddVertex(x2,y2,z2,0,0,1);
                          Face.AddVertex(x3,y3,z3,0,0,1);
                        end;
                      end;
                  end;
              end; {case}
              kind:=0;
            end;
            If st2 ='3DFACE' then
              kind:=1;  {line}
            If kind>0 then
              analyzing:=true;
          end;
        10: val(st2,x1,err);
        20: val(st2,y1,err);
        30: val(st2,z1,err);
        11: val(st2,x2,err);
        21: val(st2,y2,err);
        31: val(st2,z2,err);
        12: val(st2,x3,err);
        22: val(st2,y3,err);
        32: val(st2,z3,err);
        13: val(st2,x4,err);
        23: val(st2,y4,err);
        33: val(st2,z4,err);
      end; {of case}
    until eof(f);
  closefile(f);
end;

Procedure   Tentity.CalcNormals;
var
  Face:tface;
  i,j,numFaces,NumVertices:integer;
  inx,iny,inz:single;
begin
  i:=0;
  numFaces:=Faces.Count;
  while i<numFaces do
  begin
    j:=0;
    Face:=tface(Faces.Items[i]);
    numVertices:=Face.Vertices.Count;
    if (numVertices >= 3) then
     begin
      Face.CalcNormal(0,1,2,inx,iny,inz);   {it uses the 1st, 2nd and 3rd vertex of the face}
      while j<numVertices do
       begin
        with Tvertex(Face.Vertices[j]) do
        begin
         nx:=inx;
         ny:=iny;
         nz:=inz;
        end;
        inc(j);
      end;
     end;
    inc(i,1);
  end;
end;

Procedure   Tentity.MoveVertices(dx,dy,dz:double);
var
  j,NumPoints:integer;
begin
  {now move the vertices}
  NumPoints:=Points.Count;
  j:=0;
  while j<NumPoints do
  begin
    TPoint(Points.items[j]).x:=TPoint(Points.items[j]).x+dx;
    TPoint(Points.items[j]).y:=TPoint(Points.items[j]).y+dy;
    TPoint(Points.items[j]).z:=TPoint(Points.items[j]).z+dz;
    inc(j);
  end;
end;

Procedure   Tentity.MulVertices(dx,dy,dz:double);
var
  j,NumPoints:integer;
begin
  {now move the vertices}
  NumPoints:=Points.Count;
  j:=0;
  while j<NumPoints do
  begin
    TPoint(Points.items[j]).x:=TPoint(Points.items[j]).x*dx;
    TPoint(Points.items[j]).y:=TPoint(Points.items[j]).y*dy;
    TPoint(Points.items[j]).z:=TPoint(Points.items[j]).z*dz;
    inc(j);
  end;
end;

Procedure   Tentity.GetDiapasones;
var
  j,NumPoints:integer;
  x,y,z,
  maxx,maxy,maxz,
  minx,miny,minz:single;
begin
  maxx:=-100000000;  maxy:=-100000000;  maxz:=-100000000;
  minx:= 100000000;  miny:= 100000000;  minz:= 100000000;
  {obtain the farthest vertices}
  NumPoints:=Points.Count;
  j:=0;
  while j<NumPoints do
  begin
    x:=Tpoint(Points.items[j]).x;
    y:=Tpoint(Points.items[j]).y;
    z:=Tpoint(Points.items[j]).z;
    If x<minx then minx:=x
      else
        If x>maxX then maxX:=x;
    If y<miny then miny:=y
      else
        If y>maxy then maxy:=y;
    If z<minz then minz:=z
      else
        If z>maxz then maxz:=z;
    inc(j);
  end;

  x1:=minx; x2:=maxx;
  y1:=miny; y2:=maxy;
  z1:=minz; z2:=maxz;
end;

Procedure   Tentity.Center;
var
  j,NumPoints:integer;
  x,y,z,
  maxx,maxy,maxz,
  minx,miny,minz,
  cx,cy,cz:single;
begin
  maxx:=-100000000;  maxy:=-100000000;  maxz:=-100000000;
  minx:= 100000000;  miny:= 100000000;  minz:= 100000000;
  {obtain the farthest vertices}
  NumPoints:=Points.Count;
  j:=0;
  while j<NumPoints do
  begin
    x:=Tpoint(Points.items[j]).x;
    y:=Tpoint(Points.items[j]).y;
    z:=Tpoint(Points.items[j]).z;
    If x<minx then minx:=x
      else
        If x>maxX then maxX:=x;
    If y<miny then miny:=y
      else
        If y>maxy then maxy:=y;
    If z<minz then minz:=z
      else
        If z>maxz then maxz:=z;
    inc(j);
  end;
  {calculate the center coordinates}
  cx:=minx+(maxx-minx)/2;
  cy:=miny+(maxy-miny)/2;
  cz:=minz+(maxz-minz)/2;

  {now move the vertices}
  NumPoints:=Points.Count;
  j:=0;
  while j<NumPoints do
  begin
    TPoint(Points.items[j]).x:=TPoint(Points.items[j]).x-cx;
    TPoint(Points.items[j]).y:=TPoint(Points.items[j]).y-cy;
    TPoint(Points.items[j]).z:=TPoint(Points.items[j]).z-cz;
    inc(j);
  end;
end;

Function    Tentity.AddFace;       {add a new face to the entity}
begin
  Result:=tface.create(Self);
  Faces.add(result);
  Result.SetColor(r,g,b,a);
end;

Function Tentity.FindPoint(ix,iy,iz:single):Tpoint;    {search for a point near to x,y,z}
Var
  i,NumPoints : Integer;
  P           : TPoint;

begin
  Result:=nil;

  // This REALLY slows everything down to an unnaceptable level,
  // so I put this line in to skip it

  exit;

  numPoints:=Points.count;
  i:=0;
  while i<numPoints do
  begin
    P := Tpoint(points.items[i]);
    with P do
    If SQR(x-ix)+SQR(y-iy)+SQR(z-iz)<MinimalDistance then
    begin
      Result := P;
      Exit;
    end;
    inc(i);
  end;
end;

Function    Tentity.CreateTexture;
begin
  Texture:=Ttexture.create(self);
  Result:=texture;
  textured:=True;
end;

Procedure   Tentity.DeleteTexture;
begin
  Texture.free;
  textured:=False;
end;

Constructor T3Dtext.Create(DC:HDC; iListStart:integer);
begin
  inherited create;
  scale[1] := 1;
  scale[2] := 1;
  scale[3] := 1;
  fontScale[1] := 1;
  fontScale[2] := 1;
  fontScale[3] := 1;
  {by default use the following font style}
  FontStyle.lfHeight := -10;
  FontStyle.lfWidth := 0;
  FontStyle.lfEscapement := 0;
  FontStyle.lfOrientation := 0;
  FontStyle.lfWeight := FW_BOLD;  {FW_NORMAL, FW_BOLD}
  FontStyle.lfItalic := 0;    {use 0 for false, and 1 for true}
  FontStyle.lfUnderline := 0; {use 0 for false, and 1 for true}
  FontStyle.lfStrikeOut := 0; {use 0 for false, and 1 for true}
  FontStyle.lfCharSet := ANSI_CHARSET;
  FontStyle.lfOutPrecision := OUT_DEFAULT_PRECIS;
  FontStyle.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  FontStyle.lfQuality := DEFAULT_QUALITY;
  FontStyle.lfPitchAndFamily := DEFAULT_PITCH;
  strcopy(FontStyle.lfFaceName, 'Arial');
  ListStart:=iListStart;
  ExtrudeDepth:=0.3;  {by default}
  MyDC:=DC;
  setText('OpenGL');
  ResetFont;  {Fill the list}
end;

constructor T3Dtext.Create(otherText:T3Dtext);
begin
  inherited create;
  scale[1] := 1;
  scale[2] := 1;
  scale[3] := 1;
  fontScale[1] := otherText.fontScale[1];
  fontScale[2] := otherText.fontScale[2];
  fontScale[3] := otherText.fontScale[3];
  {by default use the following font style}
  FontStyle.lfHeight := -10;
  FontStyle.lfWidth := 0;
  FontStyle.lfEscapement := 0;
  FontStyle.lfOrientation := 0;
  FontStyle.lfWeight := FW_BOLD;  {FW_NORMAL, FW_BOLD}
  FontStyle.lfItalic := 0;    {use 0 for false, and 1 for true}
  FontStyle.lfUnderline := 0; {use 0 for false, and 1 for true}
  FontStyle.lfStrikeOut := 0; {use 0 for false, and 1 for true}
  FontStyle.lfCharSet := ANSI_CHARSET;
  FontStyle.lfOutPrecision := OUT_DEFAULT_PRECIS;
  FontStyle.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  FontStyle.lfQuality := DEFAULT_QUALITY;
  FontStyle.lfPitchAndFamily := DEFAULT_PITCH;
  strcopy(FontStyle.lfFaceName, 'Arial');
  ListStart:=otherText.ListStart;
  ExtrudeDepth:=0.3;  {by default}
  MyDC:=otherText.MyDC;
  setText('OpenGL');

  R:=otherText.R;
  G:=otherText.G;
  B:=otherText.B;
end;

Destructor  T3Dtext.Destroy;
begin
  GlDeleteLists(ListStart, 256);  {delete the display lists}
  inherited destroy;
end;

Procedure   T3Dtext.ResetFont;
var
  pagmf: PGlyphMetricsFloat;
  agmf:array [0..255] of TGlyphMetricsFloat;
  myfont:hfont;
begin
  GlDeleteLists(ListStart, 256);  {delete the display lists, if they exist}
  pagmf:=@agmf;
  myFont := CreateFontIndirect(FontStyle);
  SelectObject (MyDC, myFont);
  wglUseFontOutlines(MyDC, 0, 255, ListStart, 0.0, ExtrudeDepth, WGL_FONT_POLYGONS, pagmf);
  DeleteObject(myFont);
end;

Procedure   T3Dtext.Redraw;
begin
  {this part was copied and pasted from Tentity.redraw}
  if visible then
  begin
    GlPushMatrix();
    case wireframe of
      0:GlPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
      1:GlPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      2:GlPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    end;

    glScalef(scale[1],scale[2],scale[3]);
    glTranslatef(localPosition[1],localPosition[2],localPosition[3]);   {translate, not the object but the coordinate system}
    glRotatef(rotation[1], 1.0, 0.0, 0.0);   {same, rotate the coordinate system}
    glRotatef(rotation[2], 0.0, 1.0, 0.0);
    glRotatef(rotation[3], 0.0, 0.0, 1.0);
    glTranslatef(Position[1],Position[2],Position[3]);   {translate, not the object but the coordinate system}
    glRotatef(localRotation[1], 1.0, 0.0, 0.0);   {same, rotate the coordinate system}
    glRotatef(localRotation[2], 0.0, 1.0, 0.0);
    glRotatef(localRotation[3], 0.0, 0.0, 1.0);
    glScalef(fontScale[1],fontScale[2],fontScale[3]);

    if Assigned(Texture) then  {there is a texture assigned}
      Texture.redraw;
    If PutNames then  {If this is active then each entity has it�s own name}
    begin
      GlLoadName(id);
      GLPassThrough(id);   {it should be:  GLfloat(id), but it wouldn�t compile}
    end;
    {this is the text part}
      glColor4ub(r,g,b,A);   {set the color}
      glListBase(ListStart);
      glCallLists(textLength, GL_UNSIGNED_BYTE, text);
      glListBase(0);
    {until here}
    GlPopMatrix();
  end;
end;

Procedure   T3Dtext.SetText;
begin
  StrPcopy(text, itext);
  TextLength:=length(itext);
end;

Procedure   T3DText.localRotate;
begin
  localRotation[1]:=rx;
  localRotation[2]:=ry;
  localRotation[3]:=rz;
end;


Constructor tface.Create;
begin
  inherited create;
  Vertices:=Tlist.create;
  Owner:=iOwner;
  if Assigned(Owner.texture) then
    ApplyTexture:=true    {if there is a texture assigned, then by default the polygon will be textured too}
  else
    ApplyTexture:=false;  {the polygon won�t have a texture on it}
  r:=128;
  g:=128;
  b:=128;
  a:=255;
end;

Destructor  tface.Destroy;
Var I: Integer;
begin
  For I := 0 To Vertices.Count  - 1 Do TObject(Vertices.Items[I]).Free;
  Vertices.free;
  inherited destroy;
end;

Procedure   tface.AddVertex;
var
  Vertex:Tvertex;
  Point:Tpoint;
begin
  Vertex:=Tvertex.create(Self,nx,ny,nz);  {the vertex is always created}
  Point:=Owner.FindPoint(x,y,z);  {find a very near point to x,y,z, If found it will be used}
  If (Point=nil) or (point.r<>r) or (point.g<>g) or (point.b<>b) then  {a near, same color point was not found}
  begin
    Point:=Tpoint.Create(x,y,z);
    Point.SetColor(r,g,b);
    Owner.Points.Add(Point);
  end;
  Vertex.Point:=Point;         {reference the point...}
  Point.vertices.add(vertex);  {...and the point also references the vertex}
  inc(Point.References);  {now the vertex is referencing the point}
  Vertices.Add(Vertex);
end;

Procedure   tface.Redraw;
var
  i,num:LongInt;
  manual:boolean;
begin

  If PutNames then  {each face has it�s own name}
  begin
    GlLoadName(ActualVertexNumber);
    GlPassThrough(ActualVertexNumber);   {it should be:  GLfloat(ActualVertexNumber), but it wouldn�t compile}
  end;
  if not ApplyTexture or not Assigned(owner.texture) then
  begin
    gldisable(GL_texture_2d);
    manual:=true;
  end
    else
  begin
    glenable(gl_texture_2d);
    manual:=not owner.texture.automatic;
  end;
  num:=Vertices.count;
  if (num > 2) then
   glBegin(GL_POLYGON)
  else
  if (num = 2) then
   begin
    glBegin(GL_LINES);
    glLineWidth(3);
   end
  else
  if (num = 1) then
    glBegin(GL_POINTS);

    i:=0;
    while i<num do
      with Tvertex(Vertices.items[i]) do
      begin
        glnormal3f(nx,ny,nz);

        if Self.Owner.Trans
         Then glColor4ub(point.r Shr 1,point.g Shr 1,point.b Shr 1, a)
         Else glColor4ub(point.r,point.g,point.b, a);
        if ApplyTexture and manual then
          GlTexCoord2F(tx,1-tz);
        glvertex3f(point.x,point.y,point.z);
        inc(i);
      end;
  glEnd;
end;

Procedure tface.CalcNormal;
var
  longi,vx1,vy1,vz1,vx2,vy2,vz2:single;
begin
  vx1:=Tvertex(Vertices.items[uno]).point.x-Tvertex(Vertices.items[dos]).point.x;
  vy1:=Tvertex(Vertices.items[uno]).point.y-Tvertex(Vertices.items[dos]).point.y;
  vz1:=Tvertex(Vertices.items[uno]).point.z-Tvertex(Vertices.items[dos]).point.z;

  vx2:=Tvertex(Vertices.items[dos]).point.x-Tvertex(Vertices.items[tres]).point.x;
  vy2:=Tvertex(Vertices.items[dos]).point.y-Tvertex(Vertices.items[tres]).point.y;
  vz2:=Tvertex(Vertices.items[dos]).point.z-Tvertex(Vertices.items[tres]).point.z;
                 
  inx:=vy1*vz2 - vz1*vy2;
  iny:=vz1*vx2 - vx1*vz2;
  inz:=vx1*vy2 - vy1*vx2;

  {now reduce the vector to be unitary,  length=1}
  longi:=sqrt(inx*inx + iny*iny + inz*inz);
  If longi=0 then
    longi:=1;  {avoid zero division error}
  inx:=inx/ longi;
  iny:=iny/ longi;
  inz:=inz/ longi;
end;

Procedure tface.SetColor;
begin
  r:=iR;
  g:=iG;
  b:=iB;
  a:=iA;
end;

Procedure tface.PushColor;
begin
  r1:=R;
  g1:=G;
  b1:=B;
  a1:=A;
end;

Procedure tface.PopColor;
begin
  r:=R1;
  g:=G1;
  b:=B1;
  a:=a1;
end;

Constructor Tpoint.Create;
begin
  inherited Create;
  References:=0;
  Vertices:=Tlist.create;
  SetPosition(ix,iy,iz);
  SetColor(128,128,128);
end;

Destructor  Tpoint.Destroy;
var
  i:integer;
begin
  for i:=0 to vertices.count-1 do
    Vertices.Items[i]:=nil;
  vertices.free;
  inherited destroy;
end;

Procedure   Tpoint.SetColor(ir,ig,ib:Byte);  {set the desired color of the point}
begin
  r:=ir;
  g:=ig;
  b:=ib;
end;

Procedure TPoint.PushColor;
begin
  r1:=R;
  g1:=G;
  b1:=B;
  a1:=a;
end;

Procedure TPoint.PopColor;
begin
  r:=R1;
  g:=G1;
  b:=B1;
  a:=a1;
end;

Procedure   Tpoint.SetPosition(ix,iy,iz:single);  {move the point to a dIfferent place}
begin
  x:=ix;
  y:=iy;
  z:=iz;
end;

Constructor Tvertex.Create;
begin
  inherited Create;
  nx:=inx;
  ny:=iny;
  nz:=inz;
  Point:=nil;
  Owner:=iOwner;
  tx:=0;
  tz:=0;
end;

Procedure Tvertex.MakeCopy;
var
  NewPoint:TPoint;
begin
  If Point.References>1 then  {check If the copy is really necesary}
  begin   {the vertex is sharing the point, so let�s create its own point}
    dec(Point.References);  {this vertex won�t use that point again}
    NewPoint:=Tpoint.Create(Point.x,Point.y,Point.z);
    Owner.Owner.points.add(newPoint);
    with NewPoint do
    begin
      References:=1;   {inc the references on the new point}
      r:=Point.r;
      g:=Point.g;
      b:=Point.b;
    end;
    Point:=newPoint;
  end;  {now we are ready to set the individual values of our vertex}
end;

Procedure Tvertex.SetColor;
begin
  MakeCopy;   {If it�s necessary, the point will be duplicated so that the individual color can be modIfied}
  Point.r:=iR;
  Point.g:=iG;
  Point.b:=iB;
end;

Procedure Tvertex.SetPosition;
begin
  MakeCopy;   {If it�s necessary, the point will be duplicated so that the individual position can be modIfied}
  Point.x:=iX;
  Point.y:=iY;
  Point.z:=iZ;
end;

Constructor TTexture.Create;  {set defaults for all fields}
begin
  Owner:=iOwner;
  MyList:=glgenlists(1);  {generate an empty list for this texture}
  WrapSmode:=gl_repeat;   {tile the texture in X}
  WrapTmode:=gl_repeat;   {tile the texture in Z}
  MagFilter:=gl_nearest;  {texture using the nearest texel}
  MinFilter:=gl_nearest;  {texture using the nearest texel}
  Automatic:=False;       {by default the texture coordinates have to be calculated by hand}
  AutoGenModeX:=gl_object_linear;
  AutoGenModeZ:=gl_object_linear;
  AutoXmult[0]:=1;         AutoXmult[1]:=0;
  AutoXmult[2]:=0;         AutoXmult[3]:=0;
  AutoZmult[0]:=0;         AutoZmult[1]:=0;
  AutoZmult[2]:=1;         AutoZmult[3]:=0;
  EnvironmentMode:=GL_decal;    {like the decals in a racing car}
  EnvBlendColor[0]:=0;
  EnvBlendColor[1]:=0;
  EnvBlendColor[2]:=0;
  EnvBlendColor[3]:=255; {alpha is by default 1}
end;

Destructor  Ttexture.destroy;
begin
  GLdeleteLists(MyList,1);  {destroy the display list of the texture}
  inherited destroy;
end;

Function    TTexture.LoadTexture(st:string):shortint;
Const
  MB=19778;
type
  ptybuff=^tybuff;
  ptybuffa=^tybuffa;
  tybuff=array[1..128000] of record
                              r:byte;
                              g:byte;
                              b:byte;
                            end;
  tybuffa=array[1..128000] of record
                              r:byte;
                              g:byte;
                              b:byte;
                              a:byte;
                            end;
var
  f:file;
  Buffer2b:ptybuff;
  Buffer2:pointer;
  Buffer3b:ptybuffa;
  Buffer3:pointer;
  i:longint;
{$A-}
  header:record
    FileType:Word; {always MB}
    size:longint;
    Reserved1,
    Reserved2:word; {reserved for future purposes}
    offset:longint;  {offset to image in bytes}
  end; {header}
  BMPInfo:record
    size:longint;    {size of BMPinfo in bytes}
    width:longint;   {width of the image in pixels}
    height:longint;       {height of the image in pixels}
    planes:word;          {number of planes (always 1)}
    Colorbits:word;       {number of bits used to describe color in each pixel}
    compression:longint;  {compression used}
    ImageSize:longint;    {image size in bytes}
    XpixPerMeter:longint; {pixels per meter in X}
    YpixPerMeter:longint; {pixels per meter in Y}
    ColorUsed:longint;   {number of the color used ���???}
    Important:longint;   {number of "important" colors}
  end; {info}
{$A+}
begin
  if Not FileExists(st) then
  begin
    result:=-1;  {file not found}
    exit;
  end;
  assignfile(f,st);
  reset(f,1);
  blockread(f,header,sizeof(header));
  blockread(f,BMPinfo,sizeof(BMPinfo));
  if header.FileType <> MB then
  begin
    result:=-2;   {file type is not BMP}
    exit;
  end;
  header.size:=header.size-sizeof(header)-sizeof(BMPinfo);
  getmem(buffer2,header.size);
  getmem(buffer3,header.size*4 div 3);
  buffer2b:=ptybuff(buffer2);
  buffer3b:=ptybuffA(buffer3);
  Blockread(f,buffer2^,header.size);
  for i:=1 to header.size div 3 do
  begin
    buffer3b^[i].r:=buffer2b^[i].b;
    buffer3b^[i].g:=buffer2b^[i].g;
    buffer3b^[i].b:=buffer2b^[i].r;
    buffer3b^[i].a:=envblendcolor[3];   {obtain blend alpha from envblendcolor.alpha}
  end;
  closefile(f);
  GlNewList(MyList,gl_compile);
    glpixelstorei(gl_unpack_alignment,4);      {OpenGL 1.0 ignores this one}
    glpixelstorei(gl_unpack_row_length,0);
    glpixelstorei(gl_unpack_skip_rows,0);
    glpixelstorei(gl_unpack_skip_pixels,0);
    {for GLteximage2D the parameters are:
        gl_Texture_2d,
        level of detail (0 unless using mipmapped textures)
        components: 3 for RGB, 4 for RGBA  1 for indexed 256 color
        width, height
        border: width of the border, between 0 and 2.
        Format: gl_color_index, GL_RGB, GL_rgbA, GL_luminance are the most used
        type of the data for each pixel
        pointer to image data}
    glenable(GL_BLEND);
    glblendfunc(gl_src_alpha, gl_one_minus_src_alpha);
    gltexImage2d(gl_texture_2d,0,4,BMPinfo.width,BMPinfo.height,0,gl_rgba,gl_unsigned_byte,buffer3^);
  glendlist;
  result:=0;    {no error}
end;

Procedure TTexture.redraw;   {call the display list, it�s not really a redraw, it�s part of one}
begin
  if Automatic then  {automatic texture coordinates generation}
  begin
    gltexgeni (GL_s,gl_texture_gen_mode,AutoGenModeX);
    gltexgeniv(GL_s,gl_object_plane,addr(AutoXmult));
    gltexgeni (GL_t,gl_texture_gen_mode,AutoGenModeZ);
    gltexgeniv(GL_t,gl_object_plane,addr(AutoZmult));
    glenable(GL_TEXTURE_GEN_S);
    glenable(GL_TEXTURE_GEN_T);
  end;
  gltexparameteri(gl_texture_2d,gl_texture_wrap_s,WrapSmode);
  gltexparameteri(gl_texture_2d,gl_texture_wrap_t,WrapTmode);
  gltexparameteri(gl_texture_2d,gl_texture_mag_filter,MagFilter);
  gltexparameteri(gl_texture_2d,gl_texture_min_filter,MinFilter);
  gltexEnvi(gl_texture_env,gl_texture_env_mode,EnvironmentMode);
  glcalllist(MyList);
end;

initialization
  PutNames:=false;  {initially, the primitives will not be named}
end.
