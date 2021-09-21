# OpenZone

## About OpenZone
 OpenZone is a program that lets you create zones for use with
EQEmu

### OpenZone tutorials


#### A quick tutorial

OpenZone contains many different features and it can be daunting for
the novice user, especially if you have never used a 3D tool before. In
the process of this tutorial we'll walk through most of OpenZone's
features.

The first thing you need to understand is that OpenZone is
not a generic 3D tool. It's more of a Zone Construction
Set, and has inherent limitations on what you can make entirely within
it. However, it also has importing
capabilitiesImporting_objects_and_entire_zones, which can get around
many of its limitations. It is designed for ease of use and ease of
learning over raw power.


#### Getting the textures for the tutorial
 For this tutorial you'll need my texture set archive, which you can
find on SourceForge. It's under the EQEmu Admin section, and the
package is called "OpenZone Textures". When you get them, unzip the
archive in your  library\textures subfolder in your
OpenZone directory. At present there are several .ZIP files in the
package, and the present URL is here (external web
link)!ExecFile(http://sourceforge.net/project/showfiles.php?group_id=41381&package_id=34673).


Step 1: Create a small, flat zone


The first thing we're going to do is create a simple square zone from
scratchCreating_a\_new_zone_from_scratch. Click
File\...New from the main menu, or click on the leftmost
button on the toolbar, the one that looks like a blank sheet of
paper.


At the warning, click  Ok. You should get a dialog for
creating a new zone from scratch. Do the following:


- Set the type under Zone starting point to
"Outside".

- Leave Use BMP heightmap
unchecked.

- Leave Create invisible bounding box
unchecked

- Leave the bounding box settings alone, since we won't be
creating one.

- Set the above-water texture to "grass". If you
don't see any textures in the drop-down list, make sure that the
textures from my textureset archive are in your 
library\textures subfolder.

- Set the underwater texture to
"barrenground".

- In the bottom left, leave the Center Position
values all set to zero.

- In the bottom right, set the north-south and
east-west values to 2000. Leave the up-down
value set to zero.

- Click Ok.


OpenZone will rapidly generate a square zone that's covered in
grass.


Step 2: Switch to fly mode


The steps we're about to take won't be very visible from bird's-eye
modeLooking_at_your_zone_and_moving_around_in_it, so let's switch to
fly mode to get nice and personal with our zone. Either
click on the toolbar button that looks like a blue eye or click on
View\...Change from the main menu. The status bar at the
bottom of the window will change to let you know what mode you're
in.


Near the lower right right you should see a white circle, and three
numbers on the status bar. The circle is your compass. It should
initially be pointing up, telling you that you are facing north. The X,
Y, and Z values on the status bar represent your position, in EQ
"/loc" coordinates. X is north-south, Y is east-west, and Z is
up-down, where X increases as you move north and Y increases as you move
west. Z increases as you move up.


Step 3: Rumple the flat ground to make it more interesting


Flat as a pool table just doesn't cut it when you're outside. Let's
spruce things up a bit. Click Ground\...Rumple ground in whole
zone from the main menu.


Aha! Your raise lower amount is set to zero! Well, rumpling it by
zero wouldn't do much, would it? Let's rumple it by up
to ten units (about eight feet). Click on the Ground editing
optionsAltering_your_ground's_elevation button on the right side of the
window. At the Raise/lower amt area, either type in a
"10" or click on the spinner buttons until the value increases to 10.
Then click  Ground\...Rumple ground in whole zone from the
main menu.


It changed a little, didn't it? Press the up arrow on your keyboard to
move forweard. See? The ground isn't totally flat anymore.


Step 4: Completely enclose the zone with mountains


Let's make this zone landlocked, that is, it doesn't border an ocean
or any other large body of water. This step is a snap.


Click  Ground\...Mountainize edges from the main menu.
You should get a pop-up dialog asking you which edges to mountainize,
the mountainize amount, and whether to grow the zone outward. Click all
of the compass checkpoints, North, South, East, and West; let's make
the zone landlocked. Leave the grow outward setting unchecked and leave
the mountainize amount alone. Then click Ok.


Cool, isn't it? Look around by using the left and right arrow keys.
You're surrounded by mountains. I hope you aren't
claustrophobic!


By the way, clicking  Help\...Movement keys will give you
a pop-up telling you what the keyboard movement keys are.


Step 5: Add a zone exit to the north


Click on the Ground editing options button again (on the
right side of the window). Look for the Fly-mode crosshair
drop-down and set it to "Ground height editing". You should see a
bunch of yellow crosshairs suddenly appear, and possibly a red one right
near you. The crosshairs represent ground grid positions and they mark
the elevation at each of them, out to a certain distance. The red one is
the one that you are closest to (trust me, it's there, it might simply
be behind or below you).


Turn with your left and right arrow keys until the compass points
roughly north. Then walk forward, until the red crosshair is at the
base of the line of mountains. Try to do it such that you
are in approximately the middle of the mountain chain. Once you're
there, click Ground\...Add zone exit. You'll be presented
with a popup asking you which kind of exit you want to create. It
doesn't matter which one, so just keep the setting it gives you and
click Ok.


The wizard lowered the ground where you were and created one of those
snaky exits we all know and love. Though you might not know it, it also
created zone boundsThe_Ground_Editor so you won't be able to climb up
the sides when in-game. Where? Read on\...


Step 6: Remove the zone bound that's blocking the zone exit


The mountainize edges wizard also placed zone bounds all around your
zone to block players' movement over the mountains, and they're still
there. The one on the north is blocking the way out the exit, so we have
to change it. Open the Ground EditorThe_Ground_Editor either by clicking
on the toolbar button that looks like a snow-capped mountain or by
clicking on  Ground\...Edit ground.


The Ground Editor will appear after a few moments. The
first thing you should do is maximize it to make life easier, and then
check the By slope radio button. You should leave the
Topographic checkbox unchecked for now. After another
moment you'll have a better view of what this is all about.


You're looking down on your zone, with north at the top. Where are the
zone bounds? Well, you could check the Show zone bounds
checkbox, but that won't cut it here since we actually want to change
them, so leave it alone. Instead, click on the Bounds tab
on the left.


The bounds appeared as soon as you clicked the tab. The ground editor
knows that you need to see your bounds when you're in this tab. Each of
the yellow lines you see represents a transparent polygon that will
block progress when in-game. The little arrows in the center of each one
represent two things: first, they point to the side of the bound in
which the player can reside. Move against the little arrow, and the
bound blocks the player's progress. Second, they point out the
direction for moving \*through\* the bound. You see, zone bounds are
always one-sided: you can pass through them in one direction, but not in
the other. It's vital therefore that you draw them such that the
arrow's orientation is correct.


There are way more bounds than are necessary here. Look at the bottom
left and bottom right corners of the zone. You don't need those zone
bounds, since they're up in the mountains where we don't want the
player to go anyway. Let's get rid of them.


On the toolbox at the left, make sure the white arrow button is
depressed. This means that you're in bound selection mode (as opposed
to drawing-new-bounds mode). Click on one of the bounds you want to
discard. It should be redrawn in red. Then, click on the trash can icon
in the toolbox.


The bound will disappear. If you make a mistake, you can click on the
undo button in the toolbox. The bounds editor supports undo and redo,
and has up to twenty undo levels.


Once you've cleaned up the bottom left and bottom right corners do the
same for the upper left and upper right corners.


Now all we have to do is open up the zone exit. Select the northern
zone bound, the one at the mountain's edge that's stretching across
the central square area. Now, go to the toolbox and click on the
Snap to endpoints button. It looks like a blue line with a
red dot on the end. Each of the buttons in the toolbox has tooltips that
will tell you what they do if you hold the mouse over them.


Once you've turned on endpoint snapping, go to the rightmost endpoint
of the bound you selected and drag it until it snaps onto the left edge
of the zone exit. Remember, if you make a mistake, you can always undo.
In the worst case you can click Cancel on the bottom right
but then you'd have to start this section all over.


Once you've moved the end of the bound, we have to close off the area
to the right of the zone exit. We're going to do this by adding another
bound. In the toolbox, click the yellow button to the right of the
selection arrow button, the one called Draw new boundary.
Notice how the selection arrow button came up. You can either select
bounds or draw new ones.


Draw a bound from right to left. The way drawing works is that the
little arrow is always on the left side of whatever you're
drawing.


Go back to the toolbox and click on the selection arrow button again to
leave drawing mode. Now, drag the endpoints of the new bound until they
snap on the endpoints of other bounds to close off that area. When
you're done, you should have five bounds delineating the main square
area and several more around the zone exit.


Step 7: Add a road


We're not finished with the ground editor yet! This is a great time to
learn how to draw other textures on our zone. Click on the
Icons tab on the left side of the window.


The bounds toolbox will disappear and you'll be presented with a
graphical view of all textures under your
library\textures folder. From here you can pick a
texture, rotate it until it suits you, and paint it on the ground
grid.


Several things are worthy of note here. First, the Names
and Icons tabs are essentially the same: they present you
with textures for painting, simply in different ways: one presents the
textures as a list and the other as a thumbnail view, but they're used
in exactly the same way.


Second, you'll find that it's easier at times to work with an
unshaded view than a shaded one. You can toggle back and forth at will
by clicking any of the  Shading radio buttons at the
bottom.


Third, there is a splitter between the left area and the main grid
view. You can drag it to resize them. This lets you see more texture
thumbnails in the icon list.


Fourth, if you want to see your zone bounds anyway (useful when in an
unshaded view), check the  Show zone bounds
checkbox.


Finally, the  Topographic option is a great visual tool
for seeing areas of constant elevation and is extremely useful when
adding zone bounds. Try turning it on and off to see what it does.


Okay, so how do we paint textures? Easy. For this exercise I'm
assuming you're using the  Icons view rather than the
 Names view. All you do is click on a texture and paint it
on the map by holding the left mouse button down and dragging it around.
If you want to paint the texture at a different orientation, either
click on the icon (or list entry) repeatedly or use the rotation buttons
at the top. Then click and drag in the main grid view to paint it.
There's even a hint at the bottom of the window!


Boy, there are a lot of textures to choose from. Let me show you how to
make a simple road. Look for the last texture that looks
like a road. It should show a road that ends in the middle of the
texture. Click on it until the road starts from the middle and heads
south.


Go to the grid view at a point right below the zone exit. Here is a
place where seeing the zone bounds comes in really handy.
I recommend checking that option right now. Then, click on the grid
below the zone exit.


Cool, huh? You can drag to add multiple textures, too. Go back to the
texture list and pick the thumbnail that looks like a horizontal road
traveling all the way across. Click on it until it's vertical. Then, go
back to the grid and click-and-drag to draw a few road sections to the
south of the road end you just placed.


That's all there is to it! I've included every possible texture for
drawing roads on the grass base texture and another set for the
dark_grass texture. Hopefully as time goes by more people will create
more textures. All you have to do is add them to your
library\textures subfolder, and OpenZone will see them
when it starts up. OpenZone can read .BMP, .JPG, or .TGA textures.


For now let's keep the road short, since I want to put some other
things in the empty spaces.


One more very important thing about drawing textures: see the two radio
buttons at the bottom left, the ones labeled Land areas
and  Water areas? Remember, when you first created your
zone, when you specified two textures, "grass" and
"barrenground"?


OpenZone knows which areas are above water and which are underwater.
When painting textures with the ground editor, it automatically stops
the textures at the water's edge so you can have different textures
above and below the waterline. When you're in land area mode it won't
let you draw in underwater areas, and vice versa. We don't have any
water areas yet, but we soon will and then you'll see what I
mean.


Click Ok to save your changes and exit the  Ground
Editor. When you return to the main screen, this would be an
excellent time to save your work. You can click on the toolbar button
that looks like a floppy disk or click  File\...Save from
the main menu. Call your new scene anything you like, like
"tutorial_1".


Step 8: Make a depression in the ground


I want to add a small pool that we'll fill in with water later. You
should now be back in fly mode, and you should see your road! Using the
cursor keys, go to the northwest portion of your zone. The X coordinate
should be roughly 200 and the Y coordinate should be roughly 260.


Click on the Ground editing options button if you aren't
already there and set the Raise/lower amt to -10 (make
sure it's negative, since we are going to lower the
ground). Set the Editing radius to 4. Then click
Ground\...Raise/lower land.


A pit was created at your feet. If the raise/lower amount had been
positive, it would be a mound instead. Now let's fill the pit in with
water.


Step 9: Add a water area


Click on the toolbar button that looks like water or click
Edit\...Change water settingsAdding_water,\_lava_or_PvP_areas. The water
area dialog will appear. This is where you can control all the water (or
lava, or PvP) areas in your zone. Each of them can be square or
elliptical. We'll be creating a simple square one (don't worry,
you'll see).


Click the Add entry button at the bottom of the window. A
blank water entry will be created in the table and the edit fields will
become enabled. Then do the following:


- Under Basic stats, set it to water, water level
-6, with Has finite depth unchecked.

- Water extent should be rectangular, with the southern edge at
75, the eastern edge at 125, and both extents at 250.

- Semitransparent and tinted should be unchecked.

- Under Water/lava textures, do the following: select
"water1" from the drop-down and then click Add. Then do
the same for "water2", "water3", and "water4".

- The ground texture should be "grass".

- Set the underwater texture to "barrenground".

- Click Ok.


Not great, but not bad for a first effort. You can use the individual
ground editing controls to tweak the elevation later on. Click
hereAltering_your_ground's_elevation for more in-depth instructions on
the ground editing tools.


You can change your water settings at any time and OpenZone will
recalculate the textures for you.


If you want to see something really cool, go back to the
ground editor for a moment. Adding the water area caused OpenZone to
place grass in the above ground areas and barrenground in the underwater
areas. This is what I was talking about earlier: it treats the two
regions differently. Now exit the ground editor and go back to the main
window.


Step 10: Level a piece of ground so we can put a building on it


It's time to break out the bulldozer! Go to the southern part of your
zone, making sure not to get too close to the mountains (don't let the
crosshairs at their bases become orange). Specifically, go to the point
where your X and Y values are both approximately -250. Now click
Ground\...Flatten ground at average elevation.


The ground in this area is now totally flat, great for, say, laying the
foundation of a building. Hmmm\...


Step 11: Add a simple building


There isn't nearly enough space to cover all the nuances of creating a
building, but we'll do something simple here. I'll walk you through
the basics, but this is where the real power of OpenZone
lies and there is simply too much to put in a tutorial. Hopefully this
little introduction will get your feet wet and prepare you to learn all
the subtle nuances on your own.


Step 11.1: Lay the foundation


You construct a building in OpenZone in much the same way you do in the
real world: first you level the ground, then you lay the foundation,
then the exterior walls, then the interior walls, then the second floor,
and so on. Eventually you add the roof and proceed to interior
furnishings, etc.


We're not going to cover all of that here, but here's a small taste.
The first step is to click Ground\...Get ground height.
See that number next to Set height on the Ground
editing options pane? Round it down to the nearest
integer and write it down. That will be the bottom coordinate of your
foundation.


Now go to the "Geometry" tab on the scripted objects
panelThe_scripted_objects_panel and click on the "box" button. A popup
will appear asking you for a name for the object. Accept the default
setting, "box1". Click Ok.


You should see a small box at your feet. If you don't, maybe you're
too high up or too close to the ground. Use the Home and End keys until
it looks like you're about standing on the ground. You should see the
box. This is a good time to make the OpenZone window a little
larger.


In the object properties on the left, under "Loc" set both the X and
Y values to -250, respectively. after you do this you might have to move
around with the cursor keys to find the box again.


Once you've located the box again, set the Z value to the value you
wrote down earlier. The box will sink into the ground and might
disappear almost entirely. That's okay.


Under "Size", set the X and Y values to 40. Leave the Z value at
1.


Looks kind of plain, doesn't it? That's because it doesn't have a
texture assigned to it. Let's set one. Under Tex1, set it to eroded
concrete. It's a drop-down, so you only have to pick it. Don't use the
ellipsis (\...) button; that's for setting animated textures.


Good job; you've laid your foundation. You can wash the dried concrete
off your hands. Get ready to lay some brick instead\...


Step 11.2: Create exterior walls


Let's do something interesting first: on the Fly-mode
crosshair drop-down, change it to "Create object". The ground
crosshairs will disappear and be replaced with a single yellow one at
your feet. This one moves with you: it shows where objects will be
created when you click on them on the object toolbar. You won't
strictly need it yet, since we'll be placing our walls at precise
coordinates, but it comes in handy when you're adding frills.


Let's put up some simple brick walls. Go to the "Buildings" tab on
the scripted object toolbar and click on the "Wall" button. Accept the
default name of "wall1".


By default, new objects always start with their initial size at 1,1,1.
This makes them always appear small at first, but you'll get used to
it.


Right off the bat, set the texture to "old_bricks".


Each object type on the object toolbar is actually a script. Scripts
are tiny programs: they take parameters and create polygons in a certain
manner based on those parameters. This is what gives
OpenZone its power: it takes all the hard work away from you. The wall
script is pretty smart, and can either be solid or have "holes" in it.
What's more, one of the holes can be either a window or a door, as its
dimensions are configurable (all other holes must be windows). Let's
make a wall that faces west and has a doorway in it.


Hint: The wall script can have as many holes as you like for
additional windows. Just bear in mind that the more you have, the more
polygons there are in the zone.


Set the X location to -250 and the Y location to -210 (trust me). Set
the Z location to one plus the Z location of your foundation, since the
foundation is one unit thick. So, if your foundation's Z value was 9,
set this to 10.


Set the wall's X size to 40 and the Z size to 15. Leave the Y size at
1.


The wall extended along the north edge! Didn't we want this facing
west? You see, the wall script is directional, and defined such that it
always builds in the X direction, which is a north-south direction. We
can solve this by rotating it around its location point. That's why I
wanted it placed where I did: we're going to rotate it 90
degrees.


Rotation works by specifying the axis around which you wish to rotate
and putting in the rotation in degrees. Set the Z rotation to 270
(equivalent to -90 degrees). Ah, that's better.


Now let's add our doorway. With the wall script, the hole (door or
window) is always in the center by default if there is only one. If we
want it off-center we can set the "holeoffset" value to something
other than 0 and it will shift the hole a bit, but leave it alone for
now. Set the door property to true. Then, set the holewidth property to
8 and the holeheight property to 10. That's better.


Did you notice how some values are in bright blue and in
parentheses? One property of scripts is that their properties can have
default values. That's what these values mean: you haven't set them,
so they're reverting to what the script author thought were acceptable
defaults. You can even have properties that default to the setting of
another property. For instance, we set our foundation to only one
texture, but in reality the box script supports having different
textures for each face. Its script is coded in such a way that the
values for the other face's textures default to the first texture
setting. Had we set the others we could have the other faces take on
different textures.


In general you can revert back to a default value by either erasing
what you typed in or, in the case of a drop-down, setting it to
"(default value)". All property drop-downs have this option and this
is what it means.


Okay, let's make the rest quick. Add another wall and give it the
following settings: Loc: (-211,-210,10); Rotate: (0,0,270); Size:
(40,1,15). Set window to true and texture to old_bricks.


Step 11.3: Add a roof


I'll leave the last two walls for you to fill in. Let's add a roof
instead. On the Buildings tab, click on the roof button. Give it the
following settings: Loc: (-250,-250,25); Rotate: (0,0,0); Size:
(40,40,5). Set lrtex (which means left-right texture) to simple_wood and
fbtex (which means front-back texture) to old_bricks. Set overhangy to 1
to spruce it up a bit. The roof is an extremely powerful script and
requires some practice to learn everything it can do. I suggest starting
with something simple and playing with one option at a time to learn
what it does.


Step 12: Export your zone to .S3D


This would be a good time to save your work again. Once you've done
that, let's export everything to .S3D files. I'm assuming that you
intend for your zone to work with ZoneProxy so it doesn't have to
overwrite existing zones. All you do is click File\...Export\...Export
to .S3DExporting_your_zone. You'll be prompted for a zone name, like
"tutorial_2". Then click Ok, and watch OpenZone export
your zone. When it has finished, you'll have three new files in your
zones subfolder: tutorial_2.s3d, tutorial_2\_chr.s3d, and
tutorial_2\_obj.s3d. They're ready for use with ZoneProxy and
EQEmu.


Step 12.1: Export your zone to .XWA for use with SimpleClient


Beginning sometime in 2007 (hopefully), our own EQEmu client will be
available, called SimpleClient. It uses a new file format, called XWA,
which stands for eXtensible World Archive. .XWA files are really just
.S3D files, but they contain a new file format inside, called XWF, which
stands for eXtensible World Format. It's an entirely new file format
created especially for the EQEmu community that should be much better in
the long run for creating worlds. SimpleClient uses this file format by
default, and if you wish to use any zones you make with it then they
need to be exported to .XWA.


Exporting zones to .XWA is very similar to exporting to .S3D, with the
additional step of selecting the zone tree type. By zone trees we don't
mean trees with bark and leaves, but how zones are logically broken up
for a client. There are two types of tree, BSP (binary space partition)
and octrees. For use with SimpleClient, you should always choose BSP
trees.


Click File\...Export\...Export to .XWAExporting_your_zone. Similar
popup windows will appear that you saw when exporting to S3D. When the
additional window appears asking you to choose between BSP trees and
octrees, choose BSP trees and click Ok. Once you go
through all of the popup windows, your zone will be exported and you
will have a new .XWA file in your library\zones
subfolder.


Step 13: Add some trees


OpenZone comes with what I call the mesh libraryThe_mesh_library. In
the library\meshes subfolder are a number of files that
all have the .MSH extension. These are objects that I've already
created and are ready to be placed into your zone. We're going to add
some trees to your zone to give it that "rustic" look.


Click on the Mesh objects button on the right of the main
window. You should have seen this when you first started OpenZone: there
should be a list of objects with a small gray window below them. That
gray window is actually a small 3D viewer: if you highlight an object in
the list, you can look at it in the viewer and manipulate it just as you
would manipulate your zone from bird's-eye mode.


Highlight the "big_tree_green" object and use the left mouse button
in the small 3D viewer to spin the object around to get a good look at
it. You can also use the right mouse button to rotate it in other ways
or zoom in and out. There are also splitter bars above and to the left
of the viewer so you can change it's size.


Now, use the cursor keys to walk to an empty part of your zone. Then
click Object\...Insert selected mesh library object.
Accept the default name for the new object and click Ok
(or press enter). A tree should instantly appear in front of you! Move
around to take a look at it. I wasn't kidding when I said it was big.
Move a few paces to the side and press  Ctrl-M to add
another one. This is the shortcut key and makes adding objects from the
mesh library really easy. Go ahead and add a few trees.


If you look at the properties for any of the trees, you should see a
setting at the top called "gravity", and that it is true by default.
This means that the object will always sit on the ground. If you try to
change its Loc vlaues OpenZone will warn you that the gravity setting is
on, and changing the Z value will have no effect. The reason here is
that, even if you change the ground elevation under your tree, OpenZone
will always position the tree such that it sits on the
ground.  The trees will follow the ground when gravity is
on, which is extremely useful since it means that you can change
your ground elevation at will and not have to worry about having to
reposition your trees. OpenZone does it for you as long as gravity is
true.


You can also use the  Ground Editor to place objects,
under the Meshes tab. Sometimes doing it from the main
window is better and sometimes it's better from the Ground Editor: it
depends on what you're trying to place and how precisely you need it
positioned. If you want to place several hundred trees, for instance, I
recommend using the Ground Editor.


Step 14: Conclusion


Well, that's it for the tutorial. There's a lot to OpenZone and a lot
more to learn, but it's infinitely easier than a plain 3D tool once you
get the hang of it.
 {style="margin-top:3pt;margin-bottom:3pt;"}
13(#_NDEF_13 "{$} Mob Modeling 1: Creating a new creature race"){#_NREF_13}14(#_NDEF_14 "{#} Mob_Modeling_1:_Creating_a_new_creature_race"){#_NREF_14}15(#_NDEF_15 "{K} Mob Modeling 1: Creating a new creature race"){#_NREF_15}16(#_NDEF_16 "{+} Tutorials:000"){#_NREF_16}}Mob
Modeling 1: Creating a new creature race 


Mob Modeling 1: Creating a new creature race


In addition to creating zones, OpenZone supports creating creature
models using the free modeling program Anim8or. For this
tutorial you'll need to download and install Anim8or which at the time
of this writing is available at
www.anim8or.com!ExecFile(http://www.anim8or.com). For this tutorial we
will be assuming that you're using Anim8or 0.95. Creating a new
creature type is an incredibly involved process, and we will be tackling
it in a series of small baby steps. For the first set of tutorials, we
will use an existing creature type that is included with OpenZone.


Step 1: Installing Anim8or and setting up your project


You might already have Anim8or, or you might wish to follow the
instructions on the Anim8or site for installing it. However, there is
one unfortunate aspect of Anim8or that requires that you follow an extra
step in setting up your creature project. Anim8or stores its models in
.AN8 files, and unfortunately they hardcode the entire path of
any texture files they reference. For this set of tutorials we
will be working with a creature file that comes with OpenZone, and to
use it you need to place it and its textures in a specific folder. It
doesn't matter where you install Anim8or itself, but to continue with
the tutorial you need to follow this step closely.


For this set of tutorials we will be working with the goblin model. On
your C: drive, create a folder called 
C:\downloads\anim8or\projects. Then, from your
library\creatures subfolder, copy goblin.an8, and all
.bmp files that begin with "gob" to the
C:\downloads\anim8or\projects folder. From now on we
will be working with the files in this folder and not in
the  library\creatures subfolder.


Step 2: Making a new .AN8 file


Before we do anything, since we are making a new creature type, let's
rename the .AN8 file we just copied. Rename it to furry_goblin.an8.
It's important that it have a different name because, when we're all
done and want to use it with OpenZone again, we don't want to lose the
goblin file that we started with.


Step 3: Opening the new model in Anim8or


Okay, roll up your sleeves. It's time to start working with your
model. Start Anim8or, and before you do anything else, make sure
that the Num Lock light is lit on your keyboard. This is
critical, as if you press the keys when it's not lit, you will
inadvertently alter your model. Anim8or uses the numeric keypad for
changing your viewpoint of your model, and it really isn't possible to
work effectively without using it. Just for a quick run-down, the "5"
key on the keypad selects the front view and the arrow keys select the
left, right, above, and below views. "7" selects the rear view and the
other keys select additional views.


{bmc images\Anim8orViews.bmp}


Open furry_goblin.an8. You can do this either from within Anim8or, or
you can drag the file from Windows Explorer and drop it into Anim8or.
Either way, the goblin should appear. When it does, press the keypad
"5" key to switch to the front view. I won't walk you through
learning Anim8or itself as there are already tutorials on that, but I
will tell you what to do in Anim8or as the steps arise. For now, take a
moment to familiarize yourself with the different views by using the
numeric keypad, and switch to the front view when you're done. If you
think you accidentally changed the model, you can always close the model
without saving it and open it again.


Critical note: keeping the model ungrouped


This is not an issue in this tutorial as I've already built the model
for you, but we're making the point right up front to make sure it
sinks in. Anim8or supports grouping objects together, and while OpenZone
can handle this when it comes to importing general objects from Anim8or
files, OpenZone cannot handle grouped creature objects.
The reason is that the bone influence calculations become hopelessly
complicated in that case, and OpenZone simply doesn't take grouping
into account when figuring out which bones affect which points in your
model (if you have no idea what I'm talking about when I mention bones,
don't worry, Mob Modeling 3: Altering your creature's
structureMob_Modeling_3:\_Altering_your_creature's_structure will
explain them). When creating or importing a creature model into
Anim8or, always make sure it is totally ungrouped. If you're
ever working with a grouped object, you can use u (lowercase
u) repeatedly to make sure that everything is ungrouped. You can
then use  Ctrl-A to select everything and click
Build\...Join solids to merge everything into a single
mesh. Once again, this is not an issue for this tutorial, but it is
something that you need to keep in mind. If you mistakenly assign bone
influences to a grouped model they won't work in OpenZone, and when you
finally ungroup the object the influences will be lost and you'll have
to start over. An ounce of prevention and all that\...


Step 4: Getting rid of object variants


The goblin model has four variants. They all have the same form, but
different textures. When starting a new model, it's important that we
initially only have one variant, because when we make more variants we
need them to have the same shape under certain circumstances. This is
because of how the live client understands creature model variants. Once
we go through the step of getting rid of variants, I will explain
what's going on.


On Anim8or's main menu, click the Object item. Below the
Export\... item, you should see four object types, where
the first one is checked:


body0000_head0000 (checked)

body0001_head0001

body0002_head0002

body0003_head0003


Each of these is a separate object in the Anim8or file. We're going to
get rid of all but the first one. Select body0001_head0001. The goblin
should change to one with red skin. Then, click Edit\...Delete
Object. Click Ok when asked, and the object will be
deleted. Do the same to objects body0002_head0002 and body0003_head0003,
so that only object body0000_head0000 remains. Make sure not to
accidentally delete object body0000_head0000 or you'll have to close
the file without saving and start over; the reason is that in the next
step we're going to also delete the textures that went with those other
variants.


Step 5: Getting rid of unused texture variants


Now that only object body0000_head0000 (the green goblin) remains,
we're going to get rid of the textures that went with the variants we
just deleted, but first this would be a good time to save your work.
Click File\...Save (or press Ctrl-S) to save
your file. In fact, as you'll see below, it's vital that you do
so.


Stop and take a deep breath. It's time to explain how we are going to
use both OpenZone and Anim8or in altering your model.


It is possible to take all of the necessary steps entirely within
Anim8or, but it's incredibly tedious and error-prone. To make life a
lot easier, I've added features to OpenZone that allow it
to read, alter, and save Anim8or creature models. The Anim8or files have
to conform to some pretty stringent naming
conventionsCreating_and_exporting_creatures_with_Anim8or, but the
OpenZone features will really streamline the process. Getting rid of
texture variants is something that OpenZone is specifically designed to
do.


Altering your model in OpenZone


Everything works best if you work simultaneously in Anim8or and
OpenZone. Leave Anim8or running, having made sure to save your work. Now
start OpenZone.


Once OpenZone is running, click File\...Alter Anim8or
File\.... A window will pop up. This window lets you make all of
the changes to your file that are better done in OpenZone rather than in
Anim8or. It has a series of tabs at the top. In order, this is what they
let you do:


- File Lets you load and save an Anim8or creature file

- Textures Lets you rename or delete unused global
textures

- Global materials Lets you rename or delete unused global
materials

- Objects Lets you rename or delete unused object-specific
materials

- Figures Lets you delete unused named objects or copy bone
weight information from one named object to another

- Sequences-Adjust Lets you adjust individual animations

- Sequences-Clone Lets you copy one animation to another or
remove references to bones that don't exist


Some quick explanation:


A global texture or material is one that is available to all objects in
the file. An example of this might be a leather texture that more than
one object uses. In general, Anim8or doesn't make it very easy to get
rid of references to things that aren't used any more, so these
features have been added to OpenZone. Anim8or also doesn't make it easy
to rename things like textures or materials, and when we create new
creature models we need to use textures that conform to our naming
conventionCreating_and_exporting_creatures_with_Anim8or. OpenZone also
makes it a lot easier to move creatures up or down for individual
animation frames, and it can automatically make sure that cyclic
animations have the last frame the same as the first.


So now let's get rid of those unused texture variants. Go to the
File tab in OpenZone and browse for the Anim8or file.
Remember, we're using the one under 
C:\downloads\anim8or\projects, not the one in
library\creatures! When you've found it, click
Open.


The first step is to get rid of global materials. On the Global
materials tab, select each material where the number of
references is 0 and click Delete. OpenZone won't let you
delete a material that has at least one reference, so don't worry if
you try to delete the wrong material by mistake. When you're done, go
to the Objects tab and do the same thing for each object
in the list. You'll have to select each object before you can delete
the unused materials it references.


Once those unused materials are gone, get rid of the unused textures.
Textures are always global. Go to the Textures tab and
delete all textures with no references. Just as it is for all other
tabs, OpenZone won't let you delete a texture that is being used
somewhere. That's why it was important to get rid of unused materials
first, as materials reference textures.


While you were going through your textures, you may have noticed that
there are only limb textures for the left limbs. The goblin object uses
the same texture for the left and right limbs. This isn't strictly
necessary, but if you plan on using the same texture for both it's
cleaner to only include the texture once in your Anim8or file. You also
don't have to tag the textures for the left or right side as I did if
you're using a single texture for both sides. They're only names,
though the texture filenames are critical, as will be
explained later.


Step 6: Getting rid of phantom figure variants


This step is a bit more scary, but unfortunately a necessary one. When
you deleted the extra objects in step 4, it was supposed to also delete
certain references to those objects in what Anim8or calls
figures, but unfortunately Anim8or isn't that smart. Once
again, this would normally be very diffucult to do with just Anim8or,
but OpenZone can handle it easily.


Switch to the Figures tab. For each named object where
Object is present is No, delete it. OpenZone
won't let you get rid of a named object that is pointing to a real
object, so don't worry about deleting the wrong one.


Step 7: Saving your work


Now that the Anim8or file is nice and cleaned up, go to the
File menu and save your work. An easy way to do this is to
copy the information from the Input file: edit field to
the Output file: edit field. Whether you do this or use
the Browse\... button, make sure to click
Save when you're done!


Why we went through all this


There are several reasons, but perhaps the most compelling one is that
you presumably don't want your new creature race to look exactly like a
goblin, so before modifying the goblin model to look like something else
you should get rid of any extra variants. Whether you modified the
goblin model before or after getting rid of variants, it's a necessary
step and it's cleaner to do it first.


What the object names mean


If you recall, the names of the original four objects were:


body0000_head0000

body0001_head0001

body0002_head0002

body0003_head0003


These names have special meaning, and any creature objects you create
in Anim8or must conform to this naming convention for
OpenZone to be able to know what to do with them (Anim8or doesn't care
what name you give them, but OpenZone does). OpenZone parses the object
names to determine how they are tagged for the client, as the client
needs to know how to find certain variants. The format for object names
is explained in Creating and exporting creatures with
Anim8orCreating_and_exporting_creatures_with_Anim8or, but the gist of it
is that the format is body\<body mesh>\<body texture set>\_head\<head
mesh>\<head texture set>. For our goblin variants, the names mean that
all body and head meshes are the same and there were four distinct
texture sets (the last three of which you deleted in step 5).


Step 8: Switching back to Anim8or


This step isn't strictly necessary for this tutorial, but you'll need
to understand it for the next step in the tutorial, Mob Modeling 2:
Altering your new creatureMob_Modeling_2:\_Altering_your_new_creature.
So we're going to teach it to you now.


Now that you've saved your changes in OpenZone, leave it running and
leave the popup window up as we'll be coming back here. Switch to
Anim8or. Anim8or still has the old file loaded, so we need
to close the file in Anim8or and open it again. You need to click
 File\...Close\... in Anim8or and then open the file
again, which you should see at the bottom of the  File\...
menu. Anim8or remembers the last four files you opened in it, so it's
pretty straightforward. It's important to remember that, when you are
working this way, to  always remember to reopen the Anim8or file
in whatever program you're switching to. When switching to
Anim8or from OpenZone you need to close the file and open it again, and
when switching back to OpenZone from Anim8or you need to click the
 Load button again.


Conclusion


This concludes the first mob modeling tutorial. Your object still looks
exactly like a goblin and has only one variant, but it exists as a
separate model and could be used by OpenZone as a separate race. In the
next tutorial, we'll begin to alter it so it truly becomes a race of
its own.


Continue to modifying your creature model with Mob Modeling 2: Altering
your new creatureMob_Modeling_2:\_Altering_your_new_creature
 {style="margin-top:3pt;margin-bottom:3pt;"}
17(#_NDEF_17 "{$} Mob Modeling 2: Altering your new creature"){#_NREF_17}18(#_NDEF_18 "{#} Mob_Modeling_2:_Altering_your_new_creature"){#_NREF_18}19(#_NDEF_19 "{K} Mob Modeling 2: Altering your new creature"){#_NREF_19}20(#_NDEF_20 "{+} Tutorials:000"){#_NREF_20}}Mob
Modeling 2: Altering your new creature 


Mob Modeling 2: Altering your new creature


In Mob Modeling 1: Creating a new creature
raceMob_Modeling_1:\_Creating_a\_new_creature_race you copied the goblin
model that comes with OpenZone and turned it into its own separate model
file with all goblin variants removed. It still looks exactly like a
goblin, however, and in this tutorial we'll begin to turn it into
something else.


Step 1: Something simple: making the ears longer


Open your furry_goblin.an8 file in Anim8or (or, if you just completed
Part 1 then you're already there) and switch to the front view by
pressing "5" on the numeric keypad (remember to make sure that Num
Lock is lit on your keyboard). Now, before you go further, make sure
that Caps Lock is off. We're going to use the shortcut keys extensively
in this tutorial. There are a number of other tutorials available from
the Anim8or!ExecFile(http://www.anim8or.com) website and we're going to
assume that you know how to go through them for more information. Don't
worry, we'll be taking a very pedestrian approach and will explain what
we're doing in each step.


{bmc images\Anim8orZoomRotate.bmp} Step 1.1: Getting into the
proper view


You can make the object fit your window by pressing f (lowercase
f). Since we're only going to alter the ears, this should make
your work easier. If you want to zoom in on the head, you can use
Anim8or's pan/rotate/zoom tool to fine-tune your viewpoint. There's a
toolbar at the top with a button that looks like a circle with four
squares on it. Toggling the button gives you pan, rotate, and zoom
controls for doing this. Fully explaining the control's use is outside
the scope of this tutorial, but as a simple reference:


- left-click and drag inside the circle to rotate around the
rotate origin

- left-click and drag outside the circle to rotate around the
screen center

- right-click and drag inside the circle to pan

- right-click and drag outside the circle to move the rotate
origin in/out

- middle-click and drag (or use Alt-right-click if you don't
have a middle mouse button) anywhere to zoom in/out


You can also use  Ctrl-r to use this tool. If you use it,
remember to turn it off again by either clicking the toolbar button or
pressing Ctrl-r again. You won't be able to make any
changes to your model while the pan/rotate/zoom tool is active.


{bmc images\Anim8orPointEdit.bmp} {bmc
images\Anim8orWireframeView.bmp} Step 1.2: Getting ready to
alter points


Now that you have good frontal view of the object, switch to
Object/Point Edit Mode by pressing P (capital
P) or by clicking the button that looks like three black dots on
the left toolbar. Then, press Ctrl-W to switch to
wireframe view so you can see the individual points in the model.
Wireframe view can also be triggered by the button on the top toolbar
that looks like a little cube. That, along with the two buttons to the
right of it, allow selection of wireframe, flat shaded, and smooth
shaded views, respectively. However, only wireframe view will show you
what points or edges you have selected.


{bmc images\Anim8orSelectPoints.bmp} Step 1.3: Getting ready to
select mesh points


Press p (lowercase p) to tell Anim8or that you want to
select individual points, or click the small button on the left toolbar
that looks like a little dot. It's the leftmost button in a row of
three buttons where the two to the right of it show a line and a small
triangle respectively. These are selection mode buttons that let you
select either points, edges, or entire faces. We only want to move two
points so we want to be in point selection mode. As a side node,
e (lowercase e) lets you select edges and g
(lowercase g) (I know this is misleading) lets you select faces.
You can't use f (lowercase f) to select faces because
that fits the view to the size of your window.


{bmc images\Anim8orConstrainMovement.bmp} Step 1.4:
Constraining movement to the vertical axis


Above those buttons are three buttons labeled X,
Y, Z. These affect how you can move what
you've selected. Click them so that only the Y button is
selected. We are going to move the tips of the ears up, which
corresponds to the Y axis. As a side note, you can toggle the status of
those buttons with the x, y, or
z keys (lowercase). Anim8or is tricky in that some hotkeys
require you to use lowercase keys and others require you to use
uppercase keys, so be careful (this is why we wanted you to make sure
that Caps Lock was off). When using Anim8or, the Y axis corresponds to
up-and-down, the X axis corresponds to left-and-right, and the Z axis
corresponds to front-and-back.


{bmc images\Anim8orDragSelect.bmp} Step 1.5: Getting into
drag-selection mode


The easiest way to select the points is by dragging a selection
rectangle around them. We'll drag a single rectangle around each point,
where the first rectangle will select the first point and the second
rectangle will add the second point to our selection. Press d
(lowercase d) to tell Anim8or that you want to use 
drag-selection mode, or click the dashed-rectangle button on the
left. The other selection mode that Anim8or supports is direct selection
mode, which you would choose by pressing the a (lowercase
a) key, but we don't want to use that here.


Step 1.6: Selecting the tips of the ears


Moving near the tip of one ear, use your left mouse
button to drag a white selection rectangle around it. When you
release the mouse button, the point should be highlighted in white. If
it isn't, double-check that you are in point-selection mode with the
p (lowercase p) key and that you are in drag-selection mde
with the d (lowercase d) key.


Selecting the other point is similar, but this time use your
right mouse button to select that point. The point will be added
to your current selection such that both points are highlighted in
white.


If you make a mistake and select more points than you intended, you can
clear the selection by dragging a selection rectangle around an empty
area with your left mouse button. In general, when selecting in Anim8or
the left mouse button starts a new selection and the right mouse button
adds to the selection.


{bmc images\Anim8orMove.bmp} Step 1.7: Moving the points
up


Press the m (lowercase m) key to enter movement mode (or
click the toolbar button that looks like a crosshair with arrows on it).
Then, using the left mouse button, drag the mouse to move the points up.
They will only move up or down, since you constrained movement to the Y
axis in step 1.3. If you're not happy with the results, you can use
Anim8or's undo feature and try again.


Step 2: Changing the skin textures


Now we're going to reskin the model so it doesn't look like a goblin
anymore. For this we're going to copy part of the gnoll skin textures
and rename them so that you can change them if you want without any risk
to the existing gnoll texture set. From OpenZone's
library\creatures subfolder, copy the following files to
the C:\downloads\anim8or\projects folder:


gnlch0001.bmp

gnlch0002.bmp

gnlfa0001.bmp

gnlft0001.bmp

gnlft0002.bmp

gnlhe0001.bmp

gnlhe0003.bmp

gnlhn0001.bmp

gnllg0001.bmp

gnllg0002.bmp

gnlua0001.bmp


Once you've copied them, rename each one so that you've replaced the
"gnl" at the beginning with "fgb" for "furry goblin". As explained
in Creating and exporting creatures with
Anim8orCreating_and_exporting_creatures_with_Anim8or, texture filenames
must conform to a specific naming convention, where the
first three letters is a race type. The race type doesn't have to match
what you use when exporting the creature with OpenZone, but it has to be
distinct and consistent (for example, the wood elf textures all begin
with "aem"). At export time, OpenZone will convert the texture
filenames to the sequence you specify in its zone properties
windowEditing_overall_zone_properties. Likewise, whether you specify
"FGB" or "ZZZ" or something else in OpenZone, OpenZone will convert
the "fgb" in the texture filenames to whatever you specify.


When you're done, you should have the following texture files:


fgbch0001.bmp

fgbch0002.bmp

fgbfa0001.bmp

fgbft0001.bmp

fgbft0002.bmp

fgbhe0001.bmp

fgbhe0003.bmp

fgbhn0001.bmp

fgblg0001.bmp

fgblg0002.bmp

fgbua0001.bmp


Now, we want to change the Anim8or file so that it references the new
filenames without disturbing the model's texture coordinates or the
Anim8or file's texture identifiers. This is really
tedious in Anim8or, so we're going to use OpenZone for this. Save your
work in Anim8or and switch to OpenZone. I'm presuming that you have
just completed Part 1 of this tutorial and already have OpenZone open.
Go to OpenZone and click Load to reopen the model.


In the Textures tab, select each texture that begins with
"gob" and rename it so it begins with "fgb" instead. This works in
this case because I built the goblin and gnoll models according to the
same schema, and so the rest of the filenames happen to be the same:
both sets of texture filenames conform to OpenZone's naming
conventions, and I textured the body parts in exactly the same way (e.g.
two chest textures, two leg textures, etc.) If you were working on a
model where they didn't exactly correspond, you might have to alter
other parts of the filenames and possibly remove some unused materials
or textures on other tabs. In this case, however, this is all we need to
do.


Once you've set the model to use "fgb" textures, save it in
OpenZone's File tab. Then switch back to Anim8or, close
the file, and reopen it so Anim8or is working with the same file. If you
made a mistake Anim8or will tell you that it can't find a particular
texture. If this is the case, go back to OpenZone and make sure you got
it right. Otherwise, the model should appear in Anim8or with the gnoll
skin applied; in other words, it's furry now!


Conclusion


In this tutorial you took a baby step toward making your model
something truly different. It has longer ears than a standard goblin,
and has an entirely different skin. In Mob Modeling 3: Altering your
creature's
structureMob_Modeling_3:\_Altering_your_creature's_structure you'll
start radically altering the shape of your creature and the skeleton
underpinning it.
 {style="margin-top:3pt;margin-bottom:3pt;"}
21(#_NDEF_21 "{$} Mob Modeling 3: Altering your creature's structure"){#_NREF_21}22(#_NDEF_22 "{#} Mob_Modeling_3:_Altering_your_creature's_structure"){#_NREF_22}23(#_NDEF_23 "{K} Mob Modeling 3: Altering your creature's structure"){#_NREF_23}24(#_NDEF_24 "{+} Tutorials:000"){#_NREF_24}}Mob
Modeling 3: Altering your creature's structure 


Mob Modeling 3: Altering your creature's structure


In the last two tutorials you created a new model based on the existing
goblin model. The only structural change so far has been to the ears,
which don't move on their own. In this tutorial you'll begin to change
other parts of the model that do move, and therefore will require
changes to the model's skeleton.


What do you mean by a skeleton?


So far we've only looked at a creature model in terms of its
mesh, that is, the points and polygons that make it up.
This is the normal way of dealing with many objects in OpenZone as well,
and the idea of a mesh should be familiar. A skeleton, on
the other hand, is used to describe how a mesh can move.


A skeleton in Anim8or (or in many other tools) is composed in much the
same way as a real skeleton, with a few additions. It is composed of
"bones", where each bone generally corresponds to a real bone in a
creature. There is a bone for a bicep, a bone for a forearm, a bone for
a hand, etc.


The similarity between Anim8or skeletons and real ones ends at some
point, however. For instance, there are over 200 bones in the human
body; by contrast, an Anim8or humanoid skeleton might contain less than
two dozen. The number of bones depends on how many ways you want your
model to move \-- the point of Anim8or bones is to describe
movement, not anatomical accuracy. If your creature has
tentacles, for instance, you might use little bones to allow them to
twist and weave, even though tentacles are generally boneless.


{bmc images\Anim8orPointEdit.bmp} {bmc
images\Anim8orWireframeView.bmp} {bmc images\Anim8orSelectPoints.bmp}
{bmc images\Anim8orConstrainMovement.bmp} {bmc
images\Anim8orDragSelect.bmp} {bmc images\Anim8orMove.bmp} Step
1: Making the arms longer


We'll revisit the idea of skeletons shortly, but first let's give
ourselves a reason to do something to them. We're going to run through
this step without too much detail since you did something similar in Mob
Modeling 2: Altering your new
creatureMob_Modeling_2:\_Altering_your_new_creature.


- Open your furry goblin model and make sure that you are in
Object Mode by clicking  Mode\...Object from
the main menu.

- Switch to the top view so that you are looking
down on your model (you do this by pressing 8 on the
numeric keypad, and remember to make sure that Num Lock is lit).

- {bmc images\Anim8orPointEdit.bmp} Switch to
Object/Point Edit Mode by pressing P (capital
P).

- {bmc images\Anim8orWireframeView.bmp} Press
Ctrl-W to switch to wireframe view.

- {bmc images\Anim8orSelectPoints.bmp} Press p
(lowercase p) to switch to point-selection mode.


Let's do something simple, like making the forearms longer (do
not alter anything below the waist: in later steps I'll
explain why, and how to handle changes to the legs).


- {bmc images\Anim8orConstrainMovement.bmp} Constrain movement
to the X and Y axes with the X, Y,
Z toolbar buttons.

- {bmc images\Anim8orDragSelect.bmp} Switch to 
drag-selection mode by pressing d (lowercase
d).

- Select the left hand by dragging a rectangle around it, taking
care to not select part of the foot below.

- {bmc images\Anim8orMove.bmp} Switch to movement mode by
pressing m (lowercase m).

- Gently drag the hand so the forearm becomes a little
longer.

- {bmc images\Anim8orDragSelect.bmp} Switch back to
drag-selection mode, and do the same thing to the right hand.

- {bmc images\Anim8orUndo.bmp} Remember, if you make a mistake,
you can use Anim8or's undo feature.


Step 2: Altering the skeleton to match the new forearms


It's time to lengthen the forearm bones to match what you did to the
model mesh. We aren't going to change the animations, but
rather the skeleton behind them. Anim8or stores this in what is called
the figure.


{bmc images\Anim8orSmoothShadedView.bmp} {bmc
images\Anim8orShowBones.bmp} {bmc images\Anim8orShowObjects.bmp}
Step 2.1: Entering Figure Editing Mode


Enter figure editing mode by clicking 
Mode\...Figure on Anim8or's main menu. Then, press
Ctrl-S to switch Anim8or to Smooth Shaded view, or click
the little globe button at the top. You should see a semi-transparent
version of your furry goblin model, and you should see what look like
thin blue bones inside it. If you don't see the bones, press B
(capital B) to toggle bone display or toggle the button that
looks sort of like a spike. If you don't see your furry goblin model,
press O (capital O) to toggle object display, or toggle
the button that looks like three circles partially on top of one
another.


We want to change the length of the forearm bones, and if you look
closely at your model's arms it should look like the bones are too
short. There are two ways to lengthen them: we can edit the length
directly as a number or we can lengthen them with the mouse.


{bmc images\Anim8orSelect.bmp} Step 2.2: Lengthening a bone
using the dialog method


We'll walk through both methods. Press the a (lowercase
a) key (or select the lower arrow button) to put
Anim8or into bone selection mode and click on one of the forearm bones.
It should turn white to show you that it is selected.


Now double-click on that bone. A popup window should appear. It
describes many things about the bone, such as its name, its length, its
diameter, and how it can pivot. A bone's diameter only affects how it
is viewed in Anim8or and we don't want to change how it pivots; we only
want to make it longer.


Try entering a longer value and click Ok. The forearm bone should get a
little longer and any bones that "descend" from it (like the hand
bone) should move. Try entering different values until the length looks
right. For this model at least, the hand bone should almost extend to
the end of the hand but not poke through the end.


{bmc images\Anim8orBoneLength.bmp} Step 2.3: Lengthening a bone
using the mouse method


You could use this technigue to lengthen the other forearm as well (and
I often do this to ensure that they are the same length), but now let's
try the other technique. Select the other forearm, and this time press
the L (capital L) key to put Anim8or in a mode where you
can change a bone's length. You can also click the button that shows
two bones, one longer than the other, to enter this mode.


Now drag the tip of the bone where it meets the hand bone. Anim8or puts
a small ball-like thing on the tips of all bones to show you what to
drag. The bone should grow longer as you drag the mouse. Now you know
the primary means of changing a bone's length. When altering a
skeleton, I generally use the mouse method to set the length of one limb
and then use the dialog method to make the corresonding limb the same
length.


It takes a lot of practice to get your bones just right, and you'll
quickly find that you have to view your model from many different
directions to ensure that it is correct. Also, as you make more and more
models you'll quickly find that you have to rotate your bones to make
them fit your mesh properly. There is a great deal more information
available from the Anim8or website and the point of this tutorial is not
to teach you how to use Anim8or.


{bmc images\Anim8orRotateBones.bmp} Step 2.4: Rotating a
bone


It's possible that you might have to slightly rotate a bone to really
get it to line up with the object. Rotating a bone works much like
lengthening it with the mouse, but you have to press R (capital
R) instead. You then drag the tip of the bone to where you want
it: the left mouse button rotates it around the X axis, the right mouse
button rotates it around the Y axis, and the middle mouse button rotates
it around the Z axis. If you don't have a middle mouse button,
Alt-right button can be used as a substitute.


Be very careful when rotating bones, as it's not always obvious which
axis to use. You'll be using Anim8or's undo feature often.


Step 3: Ensuring that the bones are correctly set


Unfortunately, it's not enough to ensure that a bone "looks"
correct. It has to be correct. The reason is that bones
must ultimately affect the points in your model, and it is very easy to
get this wrong. We are now getting into the idea of  bone
influences.


Anim8or allows for two methods of telling it exactly how
bones affect your model: multi-bone skin and vertex painting. The goblin
model in this tutorial uses multi-bone skin, which is easier to use but
less exact. Vertex painting is extremely precise as you can set the
proper influencing bone for each and every vertex in your model, but is
more time-consuming. OpenZone supports both methods, and you can feel
free to use either. We will only gloss over the basics here and we'll
stick with multi-bone skin for this tutorial. However, I
strongly recommend learning how to use vertex painting: as
I've built more and more creatures I've found that it is sometimes
vital for getting things to move properly in areas where bone influences
overlap.


Critical note: keeping the model ungrouped


I won't repeat it all here, but if you've forgotten the warning on
grouped creature objects, go back to Mob Modeling 1: Creating a new
creature raceMob_Modeling_1:\_Creating_a\_new_creature_race and read the
critical note on them. This is not an issue for this tutorial as I've
already built this object and it is not grouped, but you must bear this
in mind if you're working with your own objects. Do not assign
bone influences to a grouped object! You have been warned.


{bmc images\Anim8orBoneInfluences.bmp} Bone influences:
multi-bone skin


Anim8or's multi-bone skin method defines bone influences by creating
an influence volume for each bone in your model. Actually,
to be exact, it begins with no volumes for each bone and lets you
specify which bones affect how your model warps\--for these and only
these bones it creates such a volume. And, to really
describe it correctly, I have to point out that it actually creates
two volumes \-- one entirely inside the other. The inner
volume gives the bone complete influence over the vertices
inside it, and the outer one gives the bone partial
influence. The idea is to exert complete influence over the middle of a
bone or the very end of a limb (e.g. hand or foot) and partial influence
at joints.


You can see how the bones in the furry goblin model affect the mesh by
turning on multi-bone skin display. To do this, press the S
(capital S) key and click anywhere on the translucent mesh (that
is, click anywhere on your object without clicking on a bone). You
should see a series of orange and yellow bubbles, and if you look
closely you'll see that each bubble is centered on a particular bone.
The yellow bubbles show volumes of 100% influence by a bone and the
orange bubbles show areas of partial influence. The Anim8or
documentation can explain this further, but that's the general
idea.


You shouldn't have to do this for this tutorial, but for reference you
can alter the lengths and widths of bubbles. Each bubble is defined by
something like a pipe with spherical caps on the ends. The ends of the
pipe don't have to have the same width; for instance, if you look at
the chest the top end of the pipe is wider than the bottom.


Each bubble, whether yellow or orange, has a pair of handles on each
end that can be dragged with the mouse. They can be hard to spot, but
they look like tiny squares, one on the very tip of the spherical cap
and one at its base (in the center of the end of the pipe). The handle
on the end of a cap makes the cap (and hence the end of the pipe) wider
or thinner, and the one at the cap's base moves it along the pipe
(effectively making the bubble longer or shorter). In this way you can
adjust the volume influence that each bone has.


Bone influences: vertex painting


Vertex painting works a little differently but is extremely powerful
(as an example of a model that uses it, look at the Mind Flayer model
that comes with OpenZone). It assigns each bone a color and shades every
pixel that the bone influences. The idea is that you select a bone by
clicking on it and "paint" its influence on the points that you want
it to influence. Anim8or displays a green crosshair when you are
painting influences in this manner. This is very useful when you have
bones that are very near points that you do  not want them
to influence, such as keeping the chest bone from influencing the
tentacles in the mind flayer model.


Bone influences: important notes


The multi-bone skin and vertex painting methods are mutually exclusive
\-- you have to use one or the other for your entire model. As mentioned
earlier, OpenZone supports both kinds of painting, but there are some
things you need to keep in mind. Just as for multi-bone skin, with
vertex painting Anim8or supports partial influences, where multiple
bones each have some influence on a point, and will shade a point
accordingly. OpenZone doesn't support partial influences, and will
always choose the bone that has the greatest influence on a point. So
when setting your bone influences, it's best to keep this in mind and,
when using vertex painting, you should always "max out" a bone's
influence on a point to really simulate what OpenZone will do with your
model.


When using vertex painting, there appears to be a bug in Anim8or where
sometimes a point's influence won't really change. I believe that this
is due to some bones having the same assigned color. If you have a point
that won't behave in your animations, try setting it to a bone of a
different color and then setting it to the bone you want. Nonetheless,
vertex painting is far better than bone influences in the long run, in
my opinion and you would do well to become proficient in it (remember,
you can use the Mind Flayer model as a learning tool).


Anim8or skeleton limitations and how we deal with them


You probably noticed that there are some extra bones below the waist
that don't seem to make sense. Most skeleton bones, such as the
forearms, deal with movement of pieces of your model. They might
describe how the arms, the legs, the neck, etc. bend, but it is also
necessary to describe how the entire model moves. For
instance, a jumping animation requires that you tell Anim8or how much to
move your model up or down, and a spin-kick animation requires that the
entire model spin in space. There need to be bones to handle these types
of motions, too.


Anim8or, unlike some more advanced tools, doesn't support what are
known as "telescoping" bones, that is, bones that grow longer or
shorter over time. Such a bone would make moving your model up or down
in the course of an animation very easy, but since such bones aren't
supported we have to come up with something else. It also doesn't
support "twisting" bones, and therefore we need a workaround for that,
too.


"Jacking" your model up or down


The goblin model we are using in this tutorial has a set of three bones
used to move it up or down. They are named "jack1", "jack2", and
"jack3" and connect the ground to the pelvis of your
model. Jack1 and jack2 are exactly the same length (this is critical),
whereas jack3 is very short. Together, they make moving the model up and
down in Anim8or somewhat manageable. The idea is to think of how you
might jack your car up or down in the event of a flat tire.


The reason I told you to not change anything below the waist in Step 1
is because of the fact that these three bones connect to the
pelvis. If you made the legs longer or shorter, for
instance, then the entire model would have to be moved up or down
in every keyframe of every single animation to get the
feet to properly lie on the ground. Perhaps a way might be found in the
future to make this job easier in Anim8or, but today it's a fact of
life that you'll have to do a lot of animation sequence editing if you
lengthen or shorten your model's legs. However, because of how I've
laid out the skeleton, there is an easy-to-remember system on how do do
this, and OpenZone has a feature that also makes the job much
easier.


Because jack1 and jack2 are the same length (and because of how they
were originally oriented), when editing an animation sequence (which we
won't do in this tutorial) you can move a model up or down with the
following steps:


To move a model up To move a model down

\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\--

add a to jack1.x subtract a from
jack1.x

subtract 2a from jack2.x add 2a to
jack2.x

add a to jack3.x subtract a from
jack3.x


So for example, to move your model up by a tiny amount, you might add
one degree to jack1.x and jack3.x, and subtract two degrees from
jack2.x. What this means is that you would rotate jack1 and jack3 around
their x axes by one degree and rotate jack2 around its x axis by two
degrees in the opposite direction. Because jack1 and jack2 are the same
length, the model moves straight up. You can think of jack1 and jack2 as
a pair of scissors; by doing this you've essentially opened the
scissors up a little and used the short jack3 bone to straghten the
model's orientation again. It's worth noting, however, that you
aren't limited to just this type of motion since you can rotate these
bones by any amount you want.


You should probably reread this section several times. Unless Anim8or
undergoes some advancements in the future, you will be
using the technique I've outlined here (and I wasn't the one to invent
it). You'll be using it often. You might not understand it just yet,
and perhaps you might want to hold off on rereading it until reading the
next tutorial, but trust me, you'll be back here.


Using OpenZone to make moving the model up and down easier


Performing these additions and subtractions on each keyframe can be a
real trial. OpenZone has a tab called Sequences-Adjust that has a tool
to do just this. We won't be doing it in this tutorial, but I'll
outline the steps for reference:


- On the Objects tab, select an object (any one
will do). This is very important; you can't alter a sequence unless you
do this first.

- Go to the Sequences-Adjust tab.

- Select the sequence to alter.

- Select the keyframe to change. Keyframes are marked by a black
dot, and you can only alter these.

- Set the Amount value. It's in degrees, since
you're really rotating the jack bones. Try small values such as 1
through 4.

- Click the Move up or Move down
button.


OpenZone can do this because it looks for the jack1, jack2, and jack3
bones, and if they're all found it can make these adjustments. Just as
we used naming conventions for things like objects and textures, we're
using a naming convention for certain important bones.


The Sequences-Adjust tab has two other features:


- Fixup cyclic Sequences: this will go through all sequences
whose names indicate that they are cyclic, and copy all information from
the first frame to the last frame (creating a keyframe for the last
frame if it has to). A walking animation, for example, would be a cyclic
sequence, whereas a sitting animation would not. Once again, this is
another naming convention that we use so OpenZone can know something
about a sequence. See Creating and exporting creatures with
Anim8orCreating_and_exporting_creatures_with_Anim8or for an explanation
of the sequence naming convention.


- Move legs apart: This can be used as a shortcut for objects
where the object was modeled with legs closer together (or farther
apart) than the skeleton. The idea here is that, since there are upward
of 70 humanoid animations, you should re-use the animation set when
possible. Altering a skeleton to match a model generally means that you
have to alter all of the animations, which is particularly
painful when dealing with limbs. This feature will attempt to alter all
of the animations for you (it does not alter the skeleton itself, which
you should do in Anim8or before using this feature). Using a positive
number in the Amount (degrees) box will move the legs
apart and using a negative value will move them together. It is likely
that you'll still have to manually alter many animations, but this will
take care of a lot of the work.


"Jacking" your model to twist it in space


The model also has two other bones, hjack1 and hjack2. These are used
to rotate your model horizontally in space, such as for a spin kicking
animation (where your model has to spin completely around). I won't
delve into those bones here, but the concept is the same \-- rotate the
bones to rotate and/or move your model.


As far as Anim8or is concerned, the jack and hjack bones aren't any
more special than any other bone \-- they were simply created to make
certain motions possible, and their names are part of a convention I
invented so OpenZone can know what to do with them. Once you understand
this you'll be better prepared to take full advantage of them.


Other bones


There are other bones in the model that may not be easy to see. On each
hand is a small bone that tells the client where to place handheld items
(and, importantly, how to orient them), for instance, and a bone to tell
it where to place a shield. There is a bone to tell it where to place
the name above a creature, and where to place a helmet/guild identifier.
The names of these bones are "special" (yes, another naming
convention). The difference here is that OpenZone doesn't use them:
rather, the client looks for them by name.


You aren't limited to these bones, and it's conceivable that you may
need many more (e.g. for a spider model). This tutorial centers on a
humanoid model and hence the bones are applicable. An example of
something very different would be the snake model that comes with
OpenZone, and the mind flayer model serves as a good example of a
humanoid model with "extras". The spectre model is a good example of a
humanoid model where you've taken pieces away (the legs, but it also
has many additions).


Conclusion


In this tutorial we covered making more extensive changes to your model
and how to alter the model's skeleton accordingly. Also, we covered the
repurcussions that can arise from making certain changes, and how they
can be dealt with. In general, when making a new model from an existing
one you'll have to also tweak most or all of your animations as well.
In Mob Modeling 4: Altering your
animationsMob_Modeling_4:\_Altering_your_animations we'll take a look
at animations.
 {style="margin-top:3pt;margin-bottom:3pt;"}
25(#_NDEF_25 "{$} Mob Modeling 4: Altering your animations"){#_NREF_25}26(#_NDEF_26 "{#} Mob_Modeling_4:_Altering_your_animations"){#_NREF_26}27(#_NDEF_27 "{K} Mob Modeling 4: Altering your animations"){#_NREF_27}28(#_NDEF_28 "{+} Tutorials:000"){#_NREF_28}}Mob
Modeling 4: Altering your animations 


Mob Modeling 4: Altering your animations


Anim8or calls animations sequences, and they are
accessible from its sequence editing mode. In the last
tutorial we delved into altering your model, and pointed out that some
changes (such as changing the legs) require changes to your animations.
Changing animations can be very tedious work and requires a lot of
practice. In this tutorial we'll introduce you to the sequence editing
portion of Anim8or.


{bmc images\Anim8orKeyEdit.bmp} Step 1: Entering sequence
editing mode


Enter sequence editing mode by clicking Mode\...Sequence
on Anim8or's main menu. Then, before you do anything else, click the
button on the left that looks like a key so it is highlighted in green,
or press K (capital K) to toggle it. When it's toggled it
tells Anim8or that you only want to change a bone for a single
frame in a sequence rather than for the entire sequence.


Important Note: Whenever you leave sequence editing mode and
return, Anim8or always turns keyframe editing off. Try to get yourself
in the habit of always checking the key button to make sure it's
highlighted before moving any bones. Make sure it's
green!


{bmc images\Anim8orSmoothShadedView.bmp} {bmc
images\Anim8orShowBones.bmp} {bmc images\Anim8orShowObjects.bmp} Now
that you're in sequence editing mode, make sure that both your object
and the skeleton are visible. This works exactly as it does in figure
editing mode (see the last
tutorial)Mob_Modeling_3:\_Altering_your_creature's_structure.


Step 2: Choosing an animation


Your furry goblin model has a great many animations defined for it, far
more than such a creature would ever use. They come from the full set of
animations for player models, and were built one at a time. The goblin
model inherited them because, just as you created the furry goblin model
from the existing goblin model, the goblin model was created from an
existing player model. Perhaps someday players will be allowed to play
goblins, in which case these animations will already be there, or
perhaps not. In any case, they should give you an excellent idea of what
is possible and how to accomplish it.


Clicking Sequence on the main menu will show you a
complete list of all animations defined for the model. The name of each
animation follows a set convention that OpenZone absolutely requires to
correctly export the animation:


name_cyclic_frametime_identifier


name can be just about anything you wish. OpenZone does nothing
with it; it's there so you know what the animation does.


cyclic has to be either a capital "C" or capital "N". If
it's a "C", it tells OpenZone (and, more importantly, the client)
that the animation repeats \-- it's cyclic (grin). If it's an "N"
(such as for sitting animations) then the animation does not repeat but
instead terminates at the last frame.


There is also a second, critical point to be made about
cyclic animations: the last frame must always be an exact duplicate of
the first. This can be a real pain in the neck since it means that you
have to make a duplicate frame key for any bone you change in an
animation, but it's necessary. The reason is because Anim8or doesn't
understand cyclic animations and won't correctly interpolate frame keys
with the assumption that the animation will repeat. When OpenZone sees
that an animation is cyclic, it will automatically drop the last frame
of the animation so you don't get awkward pauses in it. Because this is
so important and can be so tedious to do in Anim8or, OpenZone has a
feature to handle this for you. See Mob Modeling 3: Altering your
creature's
structureMob_Modeling_3:\_Altering_your_creature's_structure for an
explanation of how to use OpenZone to make sure that cyclic animations
end the way they begin.


frametime is the time in milliseconds that each
frame of the animation takes to complete. So, an animation with
twenty frames where frametime is 100 means that the entire animation
will take 20 x 100 = 2000 milliseconds (two seconds) to complete. The
lower the number, the faster the animation will run when played in a
client.


identifier tells the client what type of animation it is. It
always has the format of a letter followed by two numbers. Legal letter
types at this time are C, D, L, O, P, S, T. Generally speaking, they
correspond to:


identifier animation type

\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\--

 C combat

 D taking damage

 L movement

 O alternate stationary

 P stationary

 S emotes

 T spellcasting


This isn't ironclad, as, for instance, some T-series animations also
contain some combat-like animations (e.g. T07 is a flying kick
animation). The identifiers are critical, as the client, for instance,
when looking for a running animation will always look for
the animation with identifier L02 (though it has fallbacks if certain
animations are missing).


For now, let's do something relatively tame. Choose the sequence
titled "damage" with identifier "D01".


Step 3: Altering an animation


We're going to do something very simple: make the goblin rear back
just a little more when it takes damage. To do this, change your view so
you are facing the model from the left (using the 4 key on
the numeric keypad).


Step 3.1: Showing the entire bone-axis list


Before you do anything else, on the bottom left of the window is a
small area that has the name of the sequence in
it:,"damage_C\_100_D01", with what looks like a little plus sign that
indicates an expandable tree. Click the plus sign to expand the tree and
drag the little gray bar above it upward. You should see the entire list
of bones in the skeleton, further broken down by their allowed movement
axes. Use the scrollbar to go down to the line that says "ch-X". This
represents the chest bone's X axis, which on this model controls how
the chest tilts forward and back.


{bmc images\Anim8orSelect.bmp} Step 3.2: Selecting the chest
bone


Press a (lowercase a) so you can select individual bones,
and click on the bone in your model that looks like the model's spine
(it's actually the chest bone). The "ch-X", "ch-Y" and "ch-Z"
lines in the lower left should turn white to tell you that the chest
bone is selected. The little tick marks to the right represent frames in
the animation; there are ten frames, and because this is a cyclic
animation, I've set it up such that the last frame is a duplicate of
the first.


Step 3.3: Advancing to the frame we want to change


The small black dots in the frame boxes are areas where I've set a
value for a bone. Those are called keyframes (or frame keys), and we're
going to change one of them. Use the right arrow key (not the one on the
numeric keypad, but the arrow from the regular four arrow keys on your
keyboard) to advance the frame. You'll see the frame marker move to the
right. Move it so the third dot for "ch-X" is highlighed, which should
be the seventh frame. You can also do this by just clicking on the dot.
If you use the keyboard to advance frames, you should see your model
inch through the animation, and if you click on the dot instead, it
should jump directly to that frame.


{bmc images\Anim8orShowAxes.bmp} Step 3.4: Showing a bone's
allowed rotational range


We're going to tilt the chest back a little farther, but first let's
see how far it can go. Press X (capital X) to tell Anim8or
to show the rotation axis. A green arc with tick marks
will appear at the base of the chest bone, showing you its allowed
rotational range (which I've set in the figure
editorMob_Modeling_3:\_Altering_your_creature's_structure). Now we can
see what we're dealing with.


{bmc images\Anim8orRotateBones.bmp} Step 3.5: Rotating a
bone


Press R (capital R) to tell Anim8or that we want to
rotate a bone. Now, drag the tip of the chest bone where it meets the
neck with the left mouse button. You can rotate the bone
to any point you wish. Since the line we're altering is "ch-X",
we're changing the bone's X axis of rotation. In Anim8or, the three
axes look different and must be rotated differently:


axis appearance mouse button

\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\--

X green left

Y blue right

Z purple middle


So if we were altering "ch-Y", the arc would appear in
blue and we would have to use the right
mouse button to rotate it. Likewise, altering "ch-Z" would mean that
we would see a purple arc and we would have
to use the middle mouse button. A three-button mouse really is necessary
for effective work with Anim8or, though Alt-right-button can be used if
you don't have a middle mouse button.


Step 3.6: Setting a bone's rotation value exactly


Earlier I mentioned that, for cyclic animations, the last frame has to
be an exact duplicate of the first. This is done by adding frame keys
for the last frame and setting rotational values that are exactly the
same as those for the first frame. This is much more tedious than
difficult, and we'll show you how here.


To set a keyframe explicitly, you double-click the small frame box in
the bottom left for the precise frame, bone, and axis you want to
change. For instance, if we wanted to explicitly change the frame we
just altered for the "ch-X" bone/axis, we would double-click on the
little black dot. Do this now so we can see what happens.


The Key Joint Editor appears. It shows you the bone name,
the axis you're altering, its rotational range, the current frame
(which is index 6 since the very first frame is index 0), and the
current rotational value. It also has buttons for deleting a key or
adding one if there wasn't one for that frame in the first place.


If we wanted to set an explicit value we could enter it in the box and
click Ok, and in the case of making the last frame be a
duplicate of the first that is exactly what we would do (though we would
likely have to click Add Key first). It really is that
simple, though time-consuming. For this model I've done exactly that
everywhere you see a black dot in the last frame. Perhaps Anim8or may
someday understand cyclic animations or perhaps there will be an easy
way to copy the first frame to the last (the author is working on a
scripting engine), but for now this is the only way to do it in Anim8or.
I recommend using OpenZone to handle this, though switching between the
two also adds some tedium to the process.


{bmc images\Anim8orPlaySequence.bmp} Step 4: Playing the
altered animation


Playing an animation is simple: just press the Enter key.
Or, you can click the button on the left that looks like an arrow from a
VCR's "Play" button. The entire animation will play and stop on the
last frame. It might not play at the speed you expect, as our animation
speed is just a naming convention, but you can at least see it. It's
worth pointing out that the animation will stop on the
last frame, not reset to the first. Many times I've
accidentally changed the last frame of a sequence rather than the first
because I didn't realize this. Be careful and take your time. It bears
repeating.


Conclusion


With this tutorial we've gone through all of the basics of making a
creature model in Anim8or. Ultimately, though, making creatures requires
practice, practice, and more practice. Between the steps in this set of
tutorials and those available from the Anim8or community, hopefully
you'll be making your own creatures for use with OpenZone in no
time.

 {style="margin-top:3pt;margin-bottom:3pt;"}
In the next part, Mob Modeling 5: Adding
variantsMob_Modeling_5:\_Adding_variants, we'll create a variant with
darker fur.\
29(#_NDEF_29 "{$} Mob Modeling 5: Adding variants"){#_NREF_29}30(#_NDEF_30 "{#} Mob_Modeling_5:_Adding_variants"){#_NREF_30}31(#_NDEF_31 "{K} Mob Modeling 5: Using your new creature with OpenZone"){#_NREF_31}32(#_NDEF_32 "{+} Tutorials:000"){#_NREF_32}}Mob
Modeling 5: Addng variants 


Mob Modeling 5: Adding variants


Remember when we removed extra variants in Part
1Mob_Modeling_1:\_Creating_a\_new_creature_race? Well, now we're going
to add some back again. I can hear you asking already: Why on earth did
I go to the trouble of removing variants just to add them back again?
I'll tell you why: you may not have wanted any. For instance, the wight
model that comes with OpenZone doesn't have any variants \-- there is
only the one type. This may be the case for many models. Only you know
whether you will want any for your model (or as many as there were
originally). The original goblin model has four variants, which is more
than you should have for most models: extra variants means extra
textures, which means extra memory consumed from players' graphics
cards when it comes time to play the game. You should keep that in mind
when creating creatures.


Also, if you alter a model to make a new type (why
wouldn't you want to do this?), you need to get rid of
the old variants anyway. So getting rid of them really was the right
thing to do. It's a lot easier to add them now.


Step 1: Copying the furry goblin object


We're only going to create one extra variant for this tutorial, as
it's an involved process. The first step is to make a copy of the
object and give it the proper name.


If you aren't already, make sure you are in object editing
mode by clicking Mode\...Object on Anim8or's main
menu. Then select the goblin object by clicking Edit\...Select
all or pressing Ctrl-A. Then copy the object to the
clipboard with either Edit\...Copy or
Ctrl-C.


Now create a new, blank object with Object\...New.
There's nothing there yet, but now we're going to paste the object we
just copied here. Either click Edit\...Paste or press
Ctrl-V. The first object will appear just as it did
earlier.


Step 2: Giving the new variant the right name


We use naming conventions for objects and the new object needs to have
the right name so OpenZone can properly export it later. For this
variant the object's shape will be the same as earlier, but it will
have different textures. The name we choose has to reflect this. Click
Settings\...Object and enter
body0001_head0001 as the object's name. Click
Ok to close the window and then save the Anim8or
file.


Step 3: Bringing over the textures for the new variant


To keep everything simple, we're going to use more textures from the
gnoll model. From OpenZone's library\creatures
subfolder, copy the following files to the 
C:\downloads\anim8or\projects folder:


gnlch0101.bmp

gnlch0102.bmp

gnlfa0101.bmp

gnlft0101.bmp

gnlft0102.bmp

gnlhe0101.bmp

gnlhe0103.bmp

gnlhn0101.bmp

gnllg0101.bmp

gnllg0102.bmp

gnlua0101.bmp


As before, rename them all so they begin with "fgb" instead:


fgbch0101.bmp

fgbch0102.bmp

fgbfa0101.bmp

fgbft0101.bmp

fgbft0102.bmp

fgbhe0101.bmp

fgbhe0103.bmp

fgbhn0101.bmp

fgblg0101.bmp

fgblg0102.bmp

fgbua0101.bmp


{bmc images\Anim8orMaterials.bmp} Step 4: Adding texture
references


Now that you have the textures that you need, the Anim8or file needs to
know about them. There is a button on the top toolbar that looks like
four balls arranged in a square. Either click on it or press
Ctrl-M. This will display the materials palette on the
left side of the window. A series of colored balls will appear, each one
with a texture wrapped around it. Pick any one of them and double-click
on it to open up the Material Editor.


Adding more texture references is easy. On the Diffuse
row, there is a button with the letter "T" on it. Click on it to open
the Texture Selector. In this window, for each of the new
"fgb" textures above, click  Load Texture and browse for
that file. When you have loaded each of them, click Ok to
close the Texture Selector window.


We don't want to actually change the material we double-clicked on; we
only wanted to load more textures, so now click Cancel in
the Material Editor. Once it closes, save your Anim8or file again.


{bmc images\Anim8orMaterials.bmp} Step 5: Creating new
materials


The Anim8or file currently contains the following materials, and this
is what they correspond to:


fo_l00 top of the foot

fo_l0002 bottom (sole) of the foot

ca_l calves

th_l thighs (everything from the beltline to the knee)

ha_l hands

fa_l forearms

bi_l biceps

ch chest

ne neck

face_00 face

ear ears


We're going to create a new set of materials that point to the new
textures. When we're finished, they will be:


fo_l01 fgbft0101.bmp

fo_l0102 fgbft0102.bmp

ca_l01 fgbft0102.bmp

th_l01 fgblg0101.bmp

ha_l01 fgbhn0101.bmp

fa_l01 fgbfa0101.bmp

bi_l01 fgbua0101.bmp

ch01 fgbch0101.bmp

ne01 fgbch0102.bmp

face_01 fgbhe0101.bmp

ear01 fgbhe0103.bmp


We're going to walk through the first one, the top of the foot. Using
the left mouse button, drag the materials palette upward so that it
scrolls. At the very bottom there is a button marked "New".
Double-click on it to open the Material Editor.


Give the material the following settings:


Name: fo_l01

Ambient: 0.700

Diffuse: 1

Specular: 0.700

Emissive: 0.700

Rough: 32

Trans: 1 ("Trans" is misleading\...it really means opacity,
and 1 means the material is 100% opaque)

Brilliance: 1


Now to set the color settings. Make sure the button with the little
equals sign ("=") is depressed so that ambient and diffuse get the
same color. In the bottom left of the window is a color cube, with a
gradient bar just to the right of it. Left-click on the gradient bar and
drag the mouse up until the color bars above it turn white and the R, G,
B values all read 255.


The last thing is to set the texture. To the right of the
Diffuse box is a little button with a dash ("-") in it.
Click this button to open the Texture Selector. In the
texture list, select fgbft0101 and click
Ok. When you return to the Material Editor,
the dash button will have changed so it has a "T" in it, which means
that you have assigned a texture to the diffuse setting. We've finished
setting up this material, so click Ok again to close the
Material Editor and create it.


Do the same for the other materials listed above. When you have
finished with material ear01, save the Anim8or file.


Step 6: Mapping the new object variant to the new materials


This is the final step, and it can't be done in Anim8or. However,
OpenZone can make it easy:


- Load the Anim8or file in OpenZone and switch to the
Objects tab.

- Select the object, body0001_head0001.

- Under Materials used by this object, select the
fo_100 material.

- Under Available materials, select the fo_101
material.

- Click Change.


Do this for all of the materials. OpenZone won't let you change to a
material that you're already using, and it's easy to correct a
mistake, so don't worry too much. Just switch the materials to the new
set one at a time. When you're done, save the Anim8or file.


Conclusion

 {style="margin-top:3pt;margin-bottom:3pt;"}
In this tutorial you walked through the steps for creating a simple
variant where only the textures differ. This will be the norm for nearly
all creature variants you make. In the next part, we're going to use
this new creature with our zones. Proceed to the final part, Mob
Modeling 6: Using your new creature with
OpenZoneMob_Modeling_6:\_Using_your_new_creature_with_OpenZone, to use
your new creature.\
33(#_NDEF_33 "{$} Mob Modeling 6: Using your new creature with OpenZone"){#_NREF_33}34(#_NDEF_34 "{#} Mob_Modeling_6:_Using_your_new_creature_with_OpenZone"){#_NREF_34}35(#_NDEF_35 "{K} Mob Modeling 6:"){#_NREF_35}36(#_NDEF_36 "{+} Tutorials:000"){#_NREF_36}}Mob
Modeling 6: Using your new creature with OpenZone 


Mob Modeling 6: Using your new creature with OpenZone


If you've gone through the previous five parts, you should have a
furry goblin model that has slightly longer arms and ears than the
standard goblin (I never said it would be pretty), in two different fur
types (one light and one dark). Congratulations: if you made it this far
then you've tackled most of the hard parts. Now let's make it
available to export with zones.


Step 1: Making the model available to OpenZone


Copy furry_goblin.an8 and all texture names you made that begin with
"fgb" to OpenZone's library\creatures subfolder. If
you already have OpenZone running, you'll have to close it and reopen
it for it to notice the new creature, but that's really the only step
you have to take to make the model available. When OpenZone starts, it
searches its library\creatures subfolder for all .AN8
files and builds an internal list. That's how it knows what creature
models can be exported.


Step 2: Exporting the new model with your zone


This is covered at the end of Creating and exporting creatures with
Anim8orCreating_and_exporting_creatures_with_Anim8or, and all you have
to do is add the creature to your zone in the zone properties
windowEditing_overall_zone_properties. Then just save your zone and
export it. You can also create "creature-only" files this way; just
use an empty zone!


Conclusion

 {style="margin-top:3pt;margin-bottom:3pt;"}
Congratulations! You can now tackle all of the basics of creature model
creation. These tutorials concentrated on using an existing creature,
but if you intend to create model meshes from scratch or import
third-party models (e.g. from .3DS files) then the skills you learned
pretty much all carry over (with the exception of learning how to use
Anim8or itself, such as altering meshes in Object Edit
Mode , altering skeletons in Figure Edit Mode, and
altering animations in Sequence Edit Mode). Building
creature models is definitely an involved process, but this set of
tutorials should get you well on your way.\
37(#_NDEF_37 "{$} The main window"){#_NREF_37}38(#_NDEF_38 "{#} The_main_window"){#_NREF_38}39(#_NDEF_39 "{K} The main window"){#_NREF_39}40(#_NDEF_40 "{+} Introduction:000"){#_NREF_40}}The
main window 


The main window


When you first start OpenZone, you will be greeted by the main window.
You will be spending the bulk of your time here, and it contains several
features to help you build your zone:


- The main menu and main toolbar

- The scripted objects library below the main toolbar

- The main 3D viewer

- The object properties pane to the left

- The mesh library and option panels to the right

- Aditional buttons and the compass below the main 3D
viewer

- The status bar


The main menu and main toolbar


These contain the primary functions in OpenZone and from here you can
also access other parts of the program, such as the Ground
EditorThe_Ground_Editor and the water/lava/PvP properties
dialogAdding_water,\_lava_or_PvP_areas. There are also many functions
for manipulating objectsManipulating_objects as well as the
groundAltering_your_ground's_elevation. This is also where you can
create new zonesCreating_a\_new_zone_from_scratch, load and save your
work, and importImporting_objects_and_entire_zones and
exportExporting_your_zone your zones and/or zone objects. Exporting your
zone is always the final step before using it for play.


The scripted objects library


A full description of this is hereThe_scripted_objects_panel, but
suffice to say that this is used to add scripted objects to your
zone.


The main 3D viewer


The main viewer dominates the center of the window and this is where
you can viewLooking_at_your_zone_and_moving_around_in_it and work with
your zone. When you are in fly
modeLooking_at_your_zone_and_moving_around_in_it, you can click on
objects in the main viewer to select them as well as move them by
dragging them around.


The object properties pane


The pane to the left lets you change the properties of any selected
object. You can change the object's location, orientaion, and size in
this pane, as well as any other properties that are specific to
different kinds of objects. As soon as you make a change in this pane,
the change will be reflected in the main viewer.


The mesh library and option panels


The right portion of the window has a row of buttons above a panel that
runs down to the status bar. The buttons act as tabs and bring up
different panes that let you perform certain actions. The first pane
contains the mesh libraryThe_mesh_library, from which you can place
static objects into your zone. The second pane contains options for
altering the zone's groundAltering_your_ground's_elevation, and the
third pane contains OpenZone display
optionsGeneral_OpenZone_display_options.


Additional buttons and the compass


Below the main viewer is a thin panel containing six buttons that let
you move objects in small incrementsManipulating_objects. The rightmost
part of that panel contains the compass which is useful for when you are
moving around your
zoneLooking_at_your_zone_and_moving_around_in_it.


The status bar


The status bar tells you whether you are in bird's-eye
modeLooking_at_your_zone_and_moving_around_in_it or fly
modeLooking_at_your_zone_and_moving_around_in_it as well as your
position when in fly mode.
 {style="margin-top:3pt;margin-bottom:3pt;"}
41(#_NDEF_41 "{$} OpenZone folders"){#_NREF_41}42(#_NDEF_42 "{#} OpenZone_folders"){#_NREF_42}43(#_NDEF_43 "{K} OpenZone folders"){#_NREF_43}44(#_NDEF_44 "{+} Introduction:000"){#_NREF_44}}OpenZone
folders 


Folders that OpenZone uses


When you install OpenZone, the installation process will create several
subfolders underneath the base OpenZone folder. Each of these folders is
for specific types of files that OpenZone will either use or create.
This section describes what the folders are and what goes in them:


1. The OpenZone library


The library subfolder is the most important folder that
OpenZone uses. It contains the mesh library,The_mesh_library the script
library,The_scripted_objects_panel, the creature
libraryCreating_and_exporting_creatures_with_Anim8or, and all textures
for your zones.


1.1. The mesh library


The library\meshes folder contains a number of files
with the .MSH extension. Each of these is a static object that you can
place in your zone. You can add as many objects as you want to the
library by exporting them to it.


1.2. The script library


The library\scripts folder contains a number of files
that have the .SCP extension and an equal number of files with the .BMP
extension (one per script). The .SCP files are the actual scripts
themselves and the .BMP files are bitmaps that OpenZone can display on
the script library panel to represent them. The .BMP files must have the
same name as their .SCP counterparts.


1.3. OpenZone textures


Every texture you want to use with OpenZone should be placed in the
library\textures folder, with the following exception:
OpenZone supports the idea of texturesets. Texturesets are
additional folders under the  library\textures folder
where you can place textures that are specific to one zone only (e.g.
signposts). For instance, if you are creating a zone called
"deadgulch" you can create a
library\textures\deadgulch folder and place any textures
that will only be seen in that zone there. It isn't necessary to use
this feature, but it lets you keep your library\textures
folder uncluttered with textures that are only used once.


2. The scene repository


The scenes folder is the default folder where OpenZone
will look for scene files. A scene file (.SCN file)
contains everything you define for your zone and is the file format
OpenZone uses to save your work. You don't have to store your scenes
(zones) here, but it's a convenient place.


3. The zone output folder


OpenZone will place any zones you export to .S3D or .WLD in the
zones folder. When you export your zone for play you can
retrieve the .S3D files here.
 {style="margin-top:3pt;margin-bottom:3pt;"}
45(#_NDEF_45 "{$} Creating a new zone from scratch"){#_NREF_45}46(#_NDEF_46 "{#} Creating_a_new_zone_from_scratch"){#_NREF_46}47(#_NDEF_47 "{K} Creating a new zone from scratch"){#_NREF_47}48(#_NDEF_48 "{+} Using OpenZone:000"){#_NREF_48}}Creating
a new zone from scratch 


{bmc images\NewZone.bmp} Creating a new zone from scratch


Clicking  File\...New\... or clicking the toolbar button
on the main toolbar will display a popup window where you can create a
new zone from scratch. This window contains several options that let you
define the size of the zone as well as some of its properties.


Zone starting point


Here you define whether the zone contains land and what kind of land it
is. If you define it as an outside zone then OpenZone will attempt to
create a land area for your zone. If you define your zone as midair,
then OpenZone will not try to create a land area. You should select
midair if you're creating zones that consist of, for example, floating
islands or pure dungeon zones.


Check the Use BMP heightmap box if you want OpenZone to
use a heightmap when generating the land area for your zone. Heightmaps
can either be black-and-white Windows Bitmap (.BMP) files or Terragen
heightmaps (.TER). When creating the land for your zone, OpenZone
creates a grid where each grid element is 64 units across, and the
heights from the heightmap correspond to the corners of each grid
element (it will round up the size of your zone to make it fall on grid
boundaries). Therefore, if you want a perfect representation of your
heightmap, make sure that the size of your zone is a multiple of 64
times the width and height of your heightmap, minus 1 (so if the
heightmap is 100x100 elements then the size should be 64 \* 99 x 64 \*
99 or some multiple of that). OpenZone can create a zone of any size you
specify but the resulting elevations will be taken from a weighted
average of heightmap elements where necessary.


Check Create invisible bounding box if you want OpenZone
to automatically create transparent polygons around the edges of your
zone to keep people from falling off. It will only do this if you've
specified your zone as an outside zone. If you fail to do this, you can
always add zone bounds in the Ground EditorThe_Ground_Editor.


Bounding box options


This is related to the  Create invisible bounding box
option described above. The Inset value is the amount of
units in from the edges of your zone where OpenZone will create the
bounding box (the invisible polygons that keep players from walking off
the edge of the zone). It is a good idea to keep this at at least 100
units so they also can't see the edge, to preserve the immersiveness of
your zone.


The Depth option tells OpenZone how far above or below
your zone to extend the bounding boxes. This is a temporary setting, in
the sense that as you save and load your zone OpenZone always
automatically scans for the highest and lowest portions of your zone and
extends zone bounds just a little farther than that.


Above-water and underwater land textures


If you are creating an outdoor zone (whether you are using a heightmap
or not), you need to tell OpenZone which textures to use for land that
is above water and land that is below water (they can be the same if you
wish, but remember that grass generally doesn't grow underwater!). When
creating your zone, OpenZone will automatically split any polygons at
the water's edge and use the textures you've specified.


It's important to note that the water texture only applies if
you have already defined water areas for your zone. This actually
isn't very realistic when creating a zone from scratch, so don't feel
that you need to figure out where your water areas need to be at this
time. It's mainly there for when you don't like how your zone looks
and want to recreate it but want to keep your existing water settings.
This brings up another point: when entering this dialog window to create
a new zone, OpenZone does not discard your existing water
settings. This means that if you were previously working on a
zone that had water areas and really do want to start from scratch, you
should go into the Water properties
windowAdding_water,\_lava_or_PvP_areas and discard any water areas after
you've created your zone.


The Texutre set label shows your chosen texture set from
the main window. Nearly all of the time this will read "(none)" but
it's threre in case you want to keep some zone-specific textures (like
signs) in separate folders.


In this area, pick the textures to use for land that is above water and
land that is below water. If you don't pick an underwater texture then
OpenZone will just use the texture for land that is above water.


Telling OpenZone where your zone is


What you put in the  Center Position depends on how your
zone will be used. For instance, if this zone is solely for use by a
third-party client (or if you don't care about ambient zone sounds),
then leave these values at zero. However, if your zone is intended for
use with ZoneProxy and you want your zone to have correct ambient
sounds, then these values should contain unique values that shift your
zone away from all third-party zones that you intend to use.


The issue here is one of sound caching and ZoneProxy use. The standard
client caches zone sounds on a per-zone basis: this means that, when
ZoneProxy loads a new third-party zone, the zone sounds of the previous
zone are retained. The way around this is to shift all third-party zones
so that their sounds won't overlap and load all sounds for all
third-party zones at once. For instance, zone ABC could be shifted to X
= +6000 and zone DEF could be shifted to X = -6000. This way their
sounds can all be loaded at once without overlap. If you don't shift
your zone here you can always shift it later on from the main menu with
the Edit\...Shift entire zone command. Just be careful
that you don't shift too much: while the standard client will display a
zone with just about any shift, it won't display movable objects (e.g.
monsters) with coordinates exceeding +/- 32767. It's a good idea to
keep your zone shifts to within +/- 25000 or so because of this. Since
you can shift along any of the axes, this still allows for plenty of
shifted third-party zones within the allowed volume.


If you are using a third-party client or are building a server where
existing zones are discarded and overwritten (that is, you aren't using
ZoneProxy), then this entire issue is moot and you can just leave these
values at zero. It is only an issue when using ZoneProxy.


Telling OpenZone how big your zone is


This is perhaps the most important area in this window. Here you tell
OpenZone the size of your zone. The north-south extent corresponds to
the X axis (where X increases towards north) and the east-west extent
corresponds to the Y axis (where Y increases towards west). The up-down
extent corresponds to the Z axis (with Z increasing towards up).


If you are using a heightmap in creating your zone, it's important to
specify the up-down size since OpenZone will need to know the overall
elevation of the zone. If you leave it at zero, the resulting zone will
always be flat.
 {style="margin-top:3pt;margin-bottom:3pt;"}
49(#_NDEF_49 "{$} The Ground Editor"){#_NREF_49}50(#_NDEF_50 "{#} The_Ground_Editor"){#_NREF_50}51(#_NDEF_51 "{K} The Ground Editor"){#_NREF_51}52(#_NDEF_52 "{+} Using OpenZone:000"){#_NREF_52}}The
Ground Editor 


{bmc images\GroundEditor.bmp} The Ground Editor


The Ground Editor is a multipurpose window where you can
do several things:


- If your zone uses a standard OpenZone land area you can change
the polygons at each grid element,

- Add zone bounds (invisible polygons that keep players from
walking off the world or otherwise block their progress),

- Work with objects from OpenZone's mesh
libraryThe_mesh_library,

- Add zonelines (thin volumes that take players to other zones
when the player enters them),

- Add ambient sounds, and

- Split off certain areas of the ground into separate meshes
along a specific elevation.


You enter the Ground Editor by clicking Ground\...Edit
ground from the main menu or by clicking the toolbar button. The
editor consits of the map view dominating the window and an action pane
to the left. There is a splitter bar between the two that lets you make
the action pane bigger if you wish.


Display options


In addition to the tasks above, the ground editor has options that make
these tasks easier:


You can check the  Show zone bounds option to make the
editor always display the zone bounds in your zone. This way you can
always see which areas the player can enter and which areas they
can't.


Checking the  Topographic option makes the editor show
topographic elevation lines in orange. This is extremely useful when
placing zone bounds if you want the bounds to all be at a certain
elevation, and also if you want the zone to have different textures for
different elevations (e.g. grass at low elevations, rock at higher
elevations, and snow at very high elevations).


The Show other polys option makes the editor place an
overlay over the grid that displays all other polygons in your zone
(e.g. buildings, bridges, etc.), with the exception that it does
not display objects placed from the mesh library (e.g. trees).
This is useful for determining where roads should begin and end, for
instance. The reason it doesn't show objects like trees is because you
can move them around under the Meshes tab.


By default, the editor simply paints all ground textures with no
shading. However, there are two options where they can be shaded either
by the ground's slope or by the ground's
elevation. They are intended to help in determining which
areas are flat and which have a slope, as well as which areas are high
up and which aren't. These options used in conjunction with the
Topographic option make it easy to see your zone's
three-dimensional nature.


The Show hidden blocks setting, on by default, causes the
grid to display the textures for grid elements that have been hidden
(see below for hiding and un-hiding grid elements). Normally this should
be left as it is, but unchecking it causes the underlying textures to be
hidden in the grid view.


Changing the look of the land


This only applies if your zone uses land that was created from within
OpenZone (i.e. it doesn't apply if your zone was imported from
something like a .3DS file). OpenZone uses a paint tool model of
changing ground textures. The idea is to pick a texture from the list at
the left and "paint" it on the land grid by holding down the left
mouse button and dragging it around the map.


You can select textures either from the Names tab or the
 Icons tab. They do exactly the same thing, just that one
lists the names of the textures and the other shows what they actually
look like. Above either list is a window which will show the texture
you've selected. You can rotate it either by clicking on the arrow
buttons or by clicking on its entry in the list repeatedly. When you
paint it on the map, the editor will use the version you see at the
top.


If your zone has water or lava areas and has portions of the ground
that lie above the water and some that lie below it, painting on the
grid only affects either one or the other. In the bottom-left corner of
the window are two radio buttons, Land areas and 
Water areas. The first one corresponds to portions of the ground
that lie above the water and the other one corresponds to portions of
the ground that lie below the water. When you paint textures on the map,
the editor will only change those parts that correspond to the selected
option. This means that, for instance, if you have land areas selected
you can paint along a shoreline without fear of affecting the polygons
below the water, and vice versa. This works because OpenZone separates
polygons that are above and below the water into two separate ground
objects, and the radio buttons select the ground object on which to
paint.


You can hide and unhide certain grid elements by right-clicking on
them. If you've previously used the Object\...Split with ground
meshManipulating_objects main menu option to integrate a dungeon object,
for instance, you can revert back to the original, unbroken land grid
element by right-clicking on it to re-enable it and then deleting the
dungeon object and the "split" object created by OpenZone.


Adding zone bounds


Zone bounds are invisible lines that the player can't cross, like
bounds that keep them from climbing too high up mountains or walking off
the edge of the world. Every zone will at some point need zone bounds to
keep players inside. OpenZone uses zone bounds that you've defined to
create invisible polygons to keep players hemmed in.


The editor represents zone bounds with yellow lines that have an arrow
on one side. The arrow points to the area where the player is hemmed in:
when creating bounds, you want to create them such that all arrows point
in toward the "legal" portions of the zone where players should be.
Also, the arrows mean that players can pass through the bound by moving
in the direction of the arrow.


A number of buttons are available for working with bounds:


- The selection arrow lets you select a bound and do something
with it, like move it around or delete it.

- The button with a yellow line segment lets you "draw" a new
bound on the map by left-clicking and dragging the mouse.

- The button with a bunch of red dots is a "snap-to-grid"
option that makes sure that bound endpoints snap along every half-grid
boundary.

- The button with a blue line segment and red dot causes bound
endpoints to snap to the ends of other bound endpoints (a very important
option to avoid gaps between bounds).

- The two buttons with blue arrows are for undo and redo.

- The recycle bin button lets you delete a bound you've
selected.


If you have the selection arrow button checked, you can select a zone
bound by clicking anywhere on the segment. You can move a zone bound by
dragging one of its endpoints.


Adding objects from the mesh library


This tab lets you add objects from the mesh library to your zone, or
move or delete objects that you've already added. This provides an easy
way to add lots of objects quickly, such as when placing trees.


The five buttons at the top of the tab are as follows:


- The selection arrow lets you select a mesh object or more than
one by dragging a selection rectangle around them.

- The crosshair lets you place objects from the mesh
library.

- The recycle bin button lets you delete mesh object(s) you've
selected.

- The vary height button, when checked, causes any objects you
place to vary in size by a small random amount. This is useful for
organic objects like trees that shouldn't all be exactly the same
size.

- The vary angle button, when checked, causes any objects you
place to vary in angle by a small random amount. This is useful for
organic objects like trees that shouldn't all grow perfectly straight
up (it will make the trees lean a bit).


Operation is simple: to place an object, select it from the list,
select the crosshair button, and click on the map for each copy you want
to place. To move an object, click the selection arrow button, click the
object to select it, and then drag it around the map.


When you select an object type in the list it will appear in the 3D
viewer below so you can look at it. You can use the left and right mouse
buttons in the viewer to rotate it or zoom in on it. Also, any objects
you've placed that are of the same type will be highlighted in
purple.


By default, objects placed from the mesh library have their gravity
setting set to true, which means that objects you add here will always
adhere to the ground no matter where you place them. This makes this tab
very useful for placing objects like vegetation or anything else that
should always be on the ground. You can always turn a particular
object's gravity setting off from the main window.


Adding zonelines


Zonelines are lines that cause players to move to other zones when they
cross them (actually they're volumes, but OpenZone takes care of this
detail for you). You work with zonelines in much the same way that you
work with zone bounds, and the buttons for dealing with them work in
exactly the same way. However, zonelines require additional information
and the editor displays several fields for entring it.


Zonelines for most outdoor zones don't care how high up the player is:
as long as the player crosses the line, he will be moved to another
zone. For zones such as these, the Infinite Z option
should be checked. However, there can be the odd case where height does
matter, such as when there is a dungeon that extends below the ground at
that point. Another case might be where the zoneline really is for just
an underground tunnel and shouldn't affect the land above. For cases
like these, you can uncheck the infinite Z option and enter appropriate
Z values in the ZMin and ZMax fields.


When encoded in .WLD files, zonelines can also carry information as to
the destination zone ID and where the player should appear in that zone.
This is generally ignored for EQEmu since the server's database takes
care of this, but the editor will let you enter it regardless. Here you
can enter the destination zone's ID as well as the X, Y, and Z position
where the player should appear, as well as the direction the player
should face. If one or more portions are unimportant you can check the
Ignore option and the editor will encode a special value
in its place. Since EQEmu generally ignores this information anyway you
can usually leave all of these fields blank.


Adding ambient sounds


Ambient sounds are sounds the player hears when entering their affected
area. When you first click this tab, the editor will load all sounds
from your EQ folder into memory. It's important to have your EQ folder
set correctly from the main window's Edit\...Preferences
option prior to entering this tab.


The Sounds tab, like the other tabs, has several buttons
at the top. Their functions are, in order:


- The selection arrow lets you select a sound area to manipulate
it.

- The button with the yellow circle lets you add a new sound
area.

- The button with the red dots makes sure that placed sounds
snap to half-grid boundaries.

- The two buttons with blue arrows are for undo and redo,
respectively.

- the recycle bin button lets you delete a sound area you've
placed.

- The green arrow lets you listen to a sound area you've
placed.


Zone sounds work by creating references to sounds in client files. That
is, a zone sound file tells the client, "in this area, play the sound
with this name, with these characteristics". The actual sound files are
not included in zone sound files, only references to them.


There are two kinds of zone sounds: area and non-area. Area sounds
always have the same volume regardless of the player's distance from
the sound center. A good example is an ambient jungle sound. A non-area
sound is a localized sound, such as the roar of a fire, and the volume
of the played sound will decrease as the player moves away from the
epicenter.


The sound file format allows for different sounds depending on whether
it is day or night. For each sound area you place, you can specify
different day and night sounds and the appropriate sound will be played
depending on the in-game time of day.


Adding sound areas is done by selecting the add sound area button (the
button with the yellow circle), selecting a sound from the sound list,
and then clicking on the map for every sound area you want to place.
Each time you click, a new sound area will be created with a default
radius in which the sound can be heard. No sound area will be created if
you haven't first selected a sound from the list.


You can change the radius or position of a sound area by clicking the
selection arrow button and then clicking on the sound area to select it.
You select sound areas by either clicking on their center or on their
radius circle. You can move the entire sound area by dragging the center
around, or you can change just the radius by dragging from anywhere on
the radius circle.


For any sound area you've selected, you can change its properties at
any time by clicking the appropriate option or sound name. You can
listen to any sound in the list by selecting it and using the buttons at
the bottom.


Splitting the ground at a certain elevation


The ground editing grid is good for most tasks, but there are cases
where it would be nice to be able to change the ground along topographic
(elevation) lines. For instance, one might want to show snowcaps above a
certain elevation at the tops of mountains, or grass starting below a
certain elevation where the air is thicker. It also might be nice to
have different rock textures at different elevations, to show different
rock strata. While all this is possible using clever placement of
different textures, OpenZone includes an easy way to subdivide the
ground at specific elevations.


The Clip at Elevation tab contains simple controls that
make this easy: it contains an elevation edit field and slider, two
texture selection drop-downs and browse buttons, and a
Split button.


When this tab is selected, a yellow selection rectangle is painted over
the grid, with a single blue topographic line that denotes a certain
elevation. To subdivide the ground at a specific elevation, one should
follow these steps:


- Using the corner handles, drag the selection rectangle so it
encloses the area to be split.

- Using either the slider or edit field, select the desired
elevation at which to split.

- Select the upper and lower textures to be used.

- Click "Split".


The splitter works by splitting the ground and all mesh objects at the
selected elevation. For the two OpenZone ground meshes (the standard
above- and below-water meshes), grid squares to be split are hidden and
their polygons are moved into a newly created mesh. All other mesh
objects (e.g. meshes imported from .3DS files) are merely
modified.


In all affected meshes, each polygon that crosses the elevation is
split into two polygons. Those above the elevation line are assigned the
Upper Texture and those below the elevation line are
assigned the Lower Texture. Polygons that don't cross the
elevation line are unaffected. Any newly created mesh objects will be
called "GroundSplit#", where \# is a number starting with 0.


When the split is finished (it happens pretty much instantly), the
overlay is rebuilt so that the new mesh object(s) can be seen in the
editor. It is necessary to have Show other polys checked
to see them, since they aren't part of the standard ground grid.


Tips for splitting the ground at a certain elevation


- Check Topographic before setting the splitting
elevation. This will give you a better idea of where to split.

- When you attempt to drag the selection rectangle handles, you
first have to click on the rectangle to select it before dragging will
be possible.

- The split is permanent and takes effect when you click
Split; clicking Cancel will not undo it.
Take your time and make sure your settings are correct first.

- After you perform the split, check Show other
polys if you have not already done so to see the effect.

- If no imported mesh objects were split (that is, only the
standard OpenZone ground grid meshes were split), you can manually undo
the split by right-clicking on any hidden grid squares (which un-hides
them) and then deleting the "GroundSplit#" mesh objects that were
created. If any imported ground objects were also split, you will have
to manually change the textures of any affected polygons from the
polygon propertiesManipulating_objects dialog.

- It is a good idea to do this sort of splitting before
importingImporting_objects_and_entire_zones any other objects. The
reason is that the splitter affects all pure mesh objects,
including those you have imported. As a general rule of thumb it is
usually best to make sure your ground is just the way you want it before
moving on to objects that you either place or import.

- If you perform multiple splits at nearby elevations in the
same area, there may be outlier polygons that you need to fix manually
by selecting individual polygons and editing themManipulating_objects.
For example, if you perform your splits in order from highest elevation
to lowest elevation (or lowest to highest), the polygons at the extreme
end of the range will have to be changed. The reason is that the
splitter only changes those polygons that cross the elevation
line, not every polygon above or below it. This is on purpose and
allows you to add a split in between two others while preserving polygon
textures from earlier splits.
 {style="margin-top:3pt;margin-bottom:3pt;"}
53(#_NDEF_53 "{$} Adding water, lava or PvP areas"){#_NREF_53}54(#_NDEF_54 "{#} Adding_water,_lava_or_PvP_areas"){#_NREF_54}55(#_NDEF_55 "{K} Adding water, lava or PvP areas"){#_NREF_55}56(#_NDEF_56 "{+} Using OpenZone:000"){#_NREF_56}}Adding
water, lava or PvP areas 


{bmc images\EditWater.bmp} Adding water, lava or PvP
areas


This window lets you define areas containing water, lava, or PvP
(player-vs-player) areas. You can also specify textures for water and
lava areas as well as change the default land and underwater textures
for your entire zone. You enter this window by clicking
Edit\...Change water settings from the main menu or by
clicking the toolbar button.


You can change the properties of any entry you've created by selecting
it in the table at the top and changing its properties. Click
Ok to save your changes or Cancel to discard
them.


Adding a new entry


The table at the top shows any existing water, lava, or PvP areas in
your zone. You can add a new one by clicking the Add entry
button at the bottom of the window. You can delete an entry by selecting
it in the table and clicking Delete entry. If you
accidentally delete an entry by mistake, you can discard your changes by
clicking Cancel.


Setting the water level and its type


The three radio buttons on the left let you specify whether an area
contains water, lava, or is a PvP area. An important concept to
understand here is that you're really defining a volume,
a three-dimensional space that has special properties. In the case of
water and lava areas there are also polygons at the top for the surface,
but otherwise the concept is the same \-- when the player enters this
volume, something special happens. As a side note, zonelines also fall
under this category, but in OpenZone you define them in the Ground
EditorThe_Ground_Editor instead.


The Water level option lets you specify the position of
the top surface and is an absolute Z coordinate. It also applies to PvP
areas and represents the top portion of the PvP volume. If you check the
Has finite depth option you can also specify the depth of
the volume. For PvP areas this would represent where the floor is and is
the distance from the top of the volume.


Specifying the volume extents


In the Southern edge and Eastern edge
areas, enter the absolute X and Y coordinates that represent the
southeastern corner of the volume. One way to get these is to "walk"
there in fly mode and write down the coordinates shown.


The N-S extent should contain the distance from the
southernmost edge of the volume to the northermost edge. Likewise, the
E-W extent should contain the distance from the
easternmost edge to the westernmost edge.


You can make the resulting area either rectangular or elliptical in
nature. OpenZone uses 24 segments when creating elliptical
volumes.


If you importedImporting_objects_and_entire_zones a .3DS file and used
oz_water, oz_lava, or oz_pvp objects to create these areas
automatically, the areas will be classified as "irregular". You won't
be able to alter the extents of such objects.


Special features for water and lava surfaces


The options here only apply to water and lava areas (not PvP areas
since they don't have surface polygons). They let you make the surface
semitransparent if you wish or apply an extra tint to any polygons that
OpenZone will use. Semitransparent water surfaces are good for shallow
pools, for instance, where you can see the bottom.


Specifying surface textures


Water and lava areas look best when animated with multiple textures.
You can add as many textures as you like to the texture list and
OpenZone will specify them as a single animated texture when building
the zone file. A typical zone uses four textures, for example.


Changing the default ground and underwater land textures for your
entire zone


The Land and Underwater settings in the
lower right are extremely powerful. They not only specify what textures
to use when adding new water or lava areas, but they also let you change
those textures zone-wide. For example, if you created your zone without
any water areas and with no underwater texture specified, you can add a
water area in this window and set the underwater texture here. When
OpenZone creates the water area it will split all of your land polygons
at the waterline and, for any underwater areas where the texture is the
above-water texture, will change them to the underwater texture. If you
don't like the underwater texture you've chosen, you can change it
here and OpenZone will change them to the new texture. You can also use
this technique to change above-water textures zone-wide (e.g. you want
to change from "grass" to "dark-grass"). This technique preserves
any textures you've changed in the  Ground Editor by only
changing those textures that are the same as the defaults.
 {style="margin-top:3pt;margin-bottom:3pt;"}
57(#_NDEF_57 "{$} The mesh library"){#_NREF_57}58(#_NDEF_58 "{#} The_mesh_library"){#_NREF_58}59(#_NDEF_59 "{K} The mesh library"){#_NREF_59}60(#_NDEF_60 "{+} Using OpenZone:000"){#_NREF_60}}The
mesh library 


{bmc images\MeshLibrary.bmp} The mesh library


The mesh library contains a repository of objects that come with
OpenZone as well as objects you create yourself and add to the library.
It is intended to contain objects that are placed and reused often, such
as trees, furniture, lanterns, torches, huts, etc.


OpenZone has two different ways of adding objects from the mesh library
to your zone: in the main window and in the Ground
EditorThe_Ground_Editor. The help for the  Ground Editor
already has a description of how to work with mesh objects from there,
so this will center on working with them from the main window.


The starting point for working with the mesh library is to switch to
the mesh library pane on the right side of the main window. This is done
by clicking on the mesh lirary icon. The mesh library pane displays a
list of all mesh names in the library and a small 3D viewer below them.
For any object you select in the library, you can view it in the small
3D viewer. Using the mouse, you can left-drag or right-drag in the 3D
viewer to rotate or zoom in on the object to get a better look at it.
There are splitter bars above and to the left of the viewer that allow
you to resize it if you wish.


{bmc images\AddMeshObject.bmp} Adding an object from the mesh
library


Once you've picked an object you want to add to your zone, you'll
need to to change to Fly
modeLooking_at_your_zone_and_moving_around_in_it if you haven't already
done so. Once you've done this, move to where you want to place your
object and either add it with the add object toolbar button, clicking
Object\...Insert selected mesh library object, or hitting
Ctrl-M. The object will be added a small distance in front
of you. To see exactly where the object will be placed before you add
it, go to the Ground editing options
paneAltering_your_ground's_elevation and change the Fly-mode
crosshair to "Create object" (if you can't see the crosshair
after doing this, check to make sure it isn't below the ground).


Any mesh objects you place will have their Gravity
setting set to true by default. This means that, regardless of where you
place them, they will always adhere to the ground. If you try to change
their position, OpenZone will warn you that their gravity setting is on,
which means that any manual changes you make to their Z location will
have no effect. If you want to have an object sit above the ground (like
a torch), you'll need to turn its gravity setting off first.


When dealing with zones that have been entirely imported from another
format (like a .3DS file), the ground for purposes of the gravity
setting includes every object in the zone. This is because such zones
don't use standard OpenZone ground meshes and as such OpenZone doesn't
know which parts constitute ground and which don't.


The InsertMesh property deals with what to do with the
object when you export your zone. When set to False (the default), the
object will be exported as an object reference. This is an efficient way
to export objects as it means that the object mesh is only exported
once. However, there can be reasons when it is desirable to set this to
True and export the object directly into your zone geometry (or have
this happen automatically, see below for details).


{bmc images\MeshLibrary.bmp} Adding an object you've created
to the mesh library


Let's say you've created this nifty new lantern and you want to add
it to the mesh library. All you need to do is select the object and
click  Object\...Export as mesh. OpenZone will ask you
what to name the new object and it will then create an entry in the mesh
library. The object will appear in the mesh object listing and you'll
be able to place it as you would any other.


If the object you want to add is composed of more than one object,
you'll need to group them first. Also, if the name you choose for the
new object already exists in the mesh library, OpenZone will ask you if
you want to overwrite the existing object, since you cannot have two
different objects in the mesh library with the same name.


{bmc images\InsertModelOrigin.bmp} Defining the origin of an
object you intend to export to the mesh library


The Object\...Insert model origin\... option is most
useful for making equipment objects (weapons, shields, helmets, or
anything that would appear on a player or creature). For instance, the
origin (0, 0, 0 point) of a sword object should be the center of the
sword handle, and the origin of a shield object should be slightly
behind the center of the shield. This option lets you insert a special
object into OpenZone that tells it exactly where the origin should be if
you export an object to the mesh library.


When you insert a model origin, it appears as a semi-transparent blue
sphere. The way to use model origins is to place them where appropriate
and group them with the object you intend to export. For example, if you
make a sword object, you would insert a model origin object into your
scene and place it in the center of the sword handle. Then you would
group it with your sword object (or insert it into your object group if
the sword itself is a group of objects). When you export the group to
the mesh library, OpenZone will make sure the origin is at the correct
place. This way, when you export your item objectsExporting_your_zone
the objects will appear correctly when playing.


Model origins can be displayed in various ways. The display
options pane at the right side of the main windowThe_main_window
contains a drop-down that lets you select how they should be
displayed.


{bmc images\ChangeCategory.bmp} Categorizing mesh library
objects


The mesh library list is actually a hierarchical tree, and objects can
be organized into categories and sub-categories to make managing them
easier. To set an object's category, select the object in the library
list and click Object\...Change mesh library object
category\.... A popup will appear asking for the new
category.


Categories and sub-categories are created by typing a period-delimited
string. For example, to place a model of a chair in category
"furniture" and sub-category "chairs", you would enter
"furniture.chairs". OpenZone will save the object in the new category
and move it to the right place in the tree. You can move objects around
from category to category as much as you want and OpenZone will store
them properly. If you want an object to sit at the root of the tree
(that is, have no category), enter an empty string in the prompt.


Categorizing library objects has no effect at all on how they are saved
in scene files. If you change a library object's category after placing
copies of it in a scene, they will still appear in the scene. The
category only affects how the object is organized in the library
list.


{bmc images\ChangeMesh.bmp} Changing a mesh library object
you've inserted into your zone to something else


If you've inserted a particular object from the mesh library into your
zone and you want to change it to something else (for example, changing
from one chair type to another), you can easily change the object with
the Object\...Change mesh library object reference option.
Simply select the object in your zone that you want to change, select
the object that you want to change it to in the mesh library, and select
this menu option. The zone object will be changed to refer to the new
library object.


Inserting mesh library objects directly into your zone geometry rather
than as references


When you insert a mesh library object into your zone, it normally gets
inserted as a reference. This means that, when you export your zone,
only one actual copy of the mesh object is exported, and your zone
contains a reference for each time you've inserted that object. If you
place 3000 trees, for instance, OpenZone would export the tree mesh only
once and would also export 3000 references to it. This is an efficient
way to export zones and should be used most of the time.


There are cases, however, when the normal way of inserting mesh library
objects isn't a good idea. For instance, there are strict limits on how
many polygons a mesh object can have and work properly when playing
(around 1500 polygons). When an object has more than this, it tends to
not appear correctly when playing. In this case, it would be better to
insert it directly into the zone geometry, such that it becomes an
actual part of the zone when exported and not as an object reference.
The option Object\...Toggle inserting meshes into zone
geometry lets you tell OpenZone whether or not to insert meshes
as object references or directly into your zone. In either case, the
effect is visible under the  InsertMesh property in the
 properties pane.


Inserting mesh library objects as door references


All objects placed from the mesh library include a
DoorRef parameter, which is False by default. When it set
to True instead, OpenZone will only export the model to your zone but
won't explicitly include any object location information. Instead, it
will create a text file with extension .SQL in the zones
folder that contains SQL statements. The SQL statements will remove all
doors for the particular zone and add the locations of all doors (or
lifts, etc.) you've placed. In this way you can use OpenZone to place
doors even though doors are maintained by a database rather than in the
zone geometry itself.


OpenZone includes features that also make it easy to place doors if
using a common house object. You can add object placement
hotspotsAdding_object_placement_hotspots to any object you want (even if
you want to export them to the mesh library) so that adding certain
objects to them (like doors) can be easier.


OpenZone comes with a special scripted objectThe_scripted_objects_panel
called Door_Parms (under the Sql tab) that lets you supply any
additional parameters that door objects need. It operates on all placed
objects (where DoorRef is True) at its level and below. So, if a
Door_Parms object is at the top level of the zone in the object
tree, it will affect all objects in the zone. If it's in a
group, it will likewise affect everything in the group and any
sub-groups. In this way a Door_Parms object can be used to set basic
parameters for all doors in a zone and additional ones can be grouped
with doors that require specialization (such as for setting a key
required to unlock them). People with servers that use additional (or
fewer) door parameters in their database should modify this
script.
 {style="margin-top:3pt;margin-bottom:3pt;"}
61(#_NDEF_61 "{$} Looking at your zone and moving around in it"){#_NREF_61}62(#_NDEF_62 "{#} Looking_at_your_zone_and_moving_around_in_it"){#_NREF_62}63(#_NDEF_63 "{K} Looking at your zone and moving around in it"){#_NREF_63}64(#_NDEF_64 "{+} Using OpenZone:000"){#_NREF_64}}Looking
at your zone and moving around in it 


{bmc images\ViewZone.bmp} Looking at your zone


There are two ways to view your zone: Bird's-eye mode
and Fly mode.


Bird's-eye mode lets you view the entire zone from far above. It is
good for getting a quick overview of what it looks like as a whole, but
not for much else. When in this mode, you can rotate or zoom in on the
zone with the mouse by left-dragging or right-dragging in the main 3D
viewer.


Fly mode is where you will spend the bulk of your time building your
zone. It is a first-person view where you can move around your zone with
the cursor keys:


- Left arrow to turn left

- Right arrow to turn right

- Up arrow to move forward

- Down arrow to move backward

- Home to move up

- End to move down

- PgUp to pitch up

- PgDn to pitch down

- Ctrl-left to strafe left

- Ctrl-right to strafe right

- The "5" key on the numeric keypad to recenter your
view


In addition you can hold down either the Shift or Alt keys for any of
the keystrokes above to move in smaller increments (by a factor of 10),
or both to move in much smaller increments (by a factor of 100).


When in fly mode, you will see a small white circle with a line in it
below the main 3D view. This is a compass and shows you which direction
you're facing (with North at the top). Also, the status bar at the
bottom will display whether you are in birds-eye or fly mode and will
display your position when in fly mode.


{bmc images\JumpToObjectLoc.bmp} Moving to an object's
location


There is another option that you can use in fly mode to instantly go to
an object's location. After you have selected the object you're
interested in, click  View\...Jump to object location to
instantly change your location to that of the object.
 {style="margin-top:3pt;margin-bottom:3pt;"}
65(#_NDEF_65 "{$} Altering your ground's elevation"){#_NREF_65}66(#_NDEF_66 "{#} Altering_your_ground's_elevation"){#_NREF_66}67(#_NDEF_67 "{K} Altering your ground's elevation"){#_NREF_67}68(#_NDEF_68 "{+} Using OpenZone:000"){#_NREF_68}}Altering
your ground's elevation 


{bmc images\GroundEditingOptions.bmp} Altering the ground in
yor zone


When using standard OpenZone ground areas, OpenZone has some facilities
for tweaking the elevation when in Fly
modeLooking_at_your_zone_and_moving_around_in_it. You can:


- Determine the elevation at any grid node

- Set the elevation at any grid node to one retrieved in the
manner above

- Raise or lower the elevation within a certain area

- Set the elevation within a certain area equal to the highest
elevation found in that area (plateau it)

- Set the elevation within a certain area equal to the average
elevation found in that area (plateau it)

- Set the elevation within a certain area equal to the lowest
elevation found in that area (plateau it)

- Rumple the ground within a certain area

- Rumple the ground in the entire zone

- Raise mountains around the edges of your zone

- Add a zone exit to an edge of your zone

- Extend one or more edges of your zone


Working with ground grid nodes


Most of the ground editing functions deal with grid
nodes. When OpenZone creates the ground, it forms a grid with
each grid element 64 units across and uses height information at all of
the intersections in the grid to form the ground polygons. You alter the
ground by changing the elevation at one or more of these nodes, and
OpenZone will regenerate the ground polygons with the new height
information.


Ground editing requires the use of ground editing crosshairs. When in
fly mode, go to the ground editing options pane and set the
Fly-mode crosshair to "Ground height editing". This will
cause crosshairs to be displayed at all ground nodes near your position,
using the following scheme:


- The closest node will be shown in red.

- Any nodes within the editing area will be shown in
orange.

- All other nearby nodes will be shown in
yellow.


Under the ground editing options, there is an Editing
radius setting and two radio buttons, Circular and
Square. These affect the editing area for many ground
editing operations. All nodes within the editing area will show up in
either red or orange and operations that work in that area will affect
them. Yellow nodes aren't affected by editing operations and are there
merely for reference.


The Raise/lower amt setting is used for the "raise/lower
in area" operation and both rumple operations. It tells OpenZone how
much to raise or lower the ground, where positive values raise the
ground and negative values lower the ground. When rumpling the ground,
it tells OpenZone the maximum amount to raise or lower any ground node
and for rumple operations the sign of the value is irrelevant. The
negate button provides a quick way to flip the sign of the value.


{bmc images\GetElevation.bmp} {bmc images\SetElevation.bmp}
Getting and setting the elevation at grid nodes


You can retrieve the elevation at the nearest grid node (shown in red)
with these operations. When you get the elevation, it will be displayed
under Set height in the ground editing options pane. When
you use the set height operation, this height will be applied to the
nearest ground node. You can use this as a quick way to make specific
nodes all have the same elevation: walk to the node that you want them
set to, get its elevation, and then walk to each successive node until
they turn red and set their elevation to the same value.


There are two hotkeys to make this operation easy. Pressing
Ctrl-G gets the elevation at a ground node and
Ctrl-H sets the elevation at a ground node.


{bmc images\RaiseLowerGround.bmp} Raising or lowering the
elevation at nearby grid nodes


This operation uses the editing radius/type options under the ground
editing options pane as well as the raise/lower amount setting. For all
nearby nodes in the editing area (all orange or red nodes) it will raise
or lower them by the specified amount. If the amount is positive then
all nodes will be raised in elevation, and if the amount is negative
they will be lowered.


{bmc images\PlateauHighest.bmp} {bmc images\PlateauAverage.bmp} {bmc
images\PlateauLowest.bmp} "Plateauing" nearby grid nodes


OpenZone provides three different operations for making plateaus of all
orange or red nodes:


- One finds the highest elevation within the editing area and
sets them to that value.

- One finds the average elevation within the editing area and
sets them to that value.

- One finds the lowest elevation within the editing area and
sets them to that value.


They are useful for quickly leveling a plot of ground in preparation
for something you want to put there, like a building. You can think of
these operations as having a bulldozer at your fingertips.


{bmc images\RumpleGround.bmp} Rumpling the ground


Rumpling the ground means moving ground nodes up or down by small
random amounts, within the range specified by the raise/lower amount
setting. It's useful for giving your zone a more natural, "untouched"
look, as opposed to a place where people have come to settle. OpenZone
provides two operations for doing this, one that operates only on all
orange and red nodes and one that operates on every node in the entire
zone.


The zone-wide version of this is one of the few operations that you can
also perform from bird's-eye mode.


Raising mountains around the edges of the zone


Many zones have barriers around their edges instead of flat plains.
This operation lets you instantly raise mountains around any of the
edges of your zone.


When you select this option, OpenZone will display a popup window
asking you which edges need mountainizing: North, South, East, or West.
You can select any or all of these edges to mountainize. It also asks
you how high the mountains should be, and whether to extend the
mountains outward from your zone's edges or extend inward instead.
Extending outward makes your overall zone larger whereas extending
inward will keep the size the same but take away usable land area.


This works by raising the elevation around the outside of the zone, but
this operation does a couple of extra things as well. First, it rumples
the raised areas so they look something like actual mountains and not
just flat areas that are higher up. Second, it adds zone
bounds halfway up the mountain slopes to keep players from
climbing to the top. After you use this option, it might be a good idea
to go into the Ground EditorThe_Ground_Editor and check out the bounds
it added to see if they need retouching.


This is one of the few operations that you can also perform from
bird's-eye mode.


Adding a zone exit


This is meant to be done after you use the above option to mountainize
your zone's edges. This operation adds a thin zone exit corridor that
snakes through the mountains.


To properly use this option you need to walk to a specific place in fly
mode. Walk until the node at the base of the mountains that you added
turns red, at the exact place along them where you want the exit to
appear. When you select this option you'll be presented with a popup
window that displays two kinds of exit corridors, one that snakes one
way and one that snakes the other. Choose the one you want and OpenZone
will add the exit corridor. It will use the elevation at the mountains'
base where you are standing for the level of the ground in the corridor.
Also, it will create zone bounds inside the corridor to
keep players from climbing over the mountains. Now would be an excellent
time to go into the Ground Editor to check out the zone
bounds, because they will likely need some retouching.


{bmc images\ExtendEdges.bmp} Making your zone bigger


The Extend Edges option works much like the mountainize
operation but doesn't change the elevation of your zone. It merely lets
you make your zone bigger along one or more of its edges. It uses the
existing height along the edges of your zone to figure out what the new
heights should be. Like the mountainize operation, you can do this from
either bird's-eye or fly mode.
 {style="margin-top:3pt;margin-bottom:3pt;"}
69(#_NDEF_69 "{$} Editing overall zone properties"){#_NREF_69}70(#_NDEF_70 "{#} Editing_overall_zone_properties"){#_NREF_70}71(#_NDEF_71 "{K} Editing overall zone properties"){#_NREF_71}72(#_NDEF_72 "{+} Using OpenZone:000"){#_NREF_72}}Editing
overall zone properties 


{bmc images\ZoneProperties.bmp} Editing overall zone
properties


At the top of the  Zone Properties window are edit fields
for your zone's long name and its short name. The long name corresponds
to the name users will see when they enter your zone (e.g. Canyons of
Gur) and the short name corresponds to the name of the zone files (e.g.
gurcanyons).


The Zone type radio buttons affect how your zone handles
lighting. Outdoor zones respond to sunlight and will grow lighter as the
sun rises and darker as it sets. Indoor zones will always have constant
ambient lighting, day or night.


Adding extra mesh objects to your zone


In addition to mesh library objectsThe_mesh_library that are always
shown in your zone, you might need to add extra objects to your zone as
well, such as doors (or anything else that the EQEmu "doors" table
might ask for that you're providing, but usually it's just doors). To
add extra objects, select the object type under Meshes in mesh
library and click the Add button. The mesh will be
removed from the mesh list (since you can't add it more than once) and
added to the Extra meshes in zone list. Do this for every
mesh type you need to add. When your zone is exported these objects will
come along for the ride. They will be the first mesh objects exported,
before any other mesh objects in your zone.


Adding creatures to your zone


If you have added Anim8or creature files to the
library\creatures folder, they will show up in the
Creatures in library list. To add creatures, highlight a
model and click the the Add button. You will be prompted
for a race identifier, e.g. "ELF", "ELM", "GOB", etc. The race
identifier should correspond with the race number that the game server
uses for any spawned creatures.


More information on creating creature models with Anim8or can be found
hereCreating_and_exporting_creatures_with_Anim8or .
 {style="margin-top:3pt;margin-bottom:3pt;"}
73(#_NDEF_73 "{$} Manipulating objects"){#_NREF_73}74(#_NDEF_74 "{#} Manipulating_objects"){#_NREF_74}75(#_NDEF_75 "{K} Manipulating objects"){#_NREF_75}76(#_NDEF_76 "{+} Using OpenZone:000"){#_NREF_76}}Manipulating
objects 

Selecting objects


There are two ways to select an object, either by picking it in the
object tree or by clicking directly on it when in fly
modeLooking_at_your_zone_and_moving_around_in_it. You can select
multiple objects either by using Shift-click or Ctrl-click in the object
tree, or by right-clicking on them in the 3D viewer to add or remove
them from the list of selected objects. Any objects you select will have
yellow bounding rectangles placed around them so you can see what you
selected.


If you select an object by clicking on it and that object also happens
to be a mesh that you've imported, the specific polygon you clicked on
will also be highlighted in green. See Manipulating Mesh
Objects below for more on working with mesh objects and their
polygons.


{bmc images\editcut.bmp} {bmc images\editcopy.bmp} {bmc
images\editpaste.bmp}  Working with the clipboard


OpenZone has the standard cut, copy, and paste clipboard functions.
They work on all selected objects so if you have selected more than one
object then copies will be made of all of them. The copies will be
positioned at a slightly different position than the originals so you
can see them.


{bmc images\Rename.bmp} Renaming an object


If you have selected only one object you can rename it by clicking
Object\...Rename. OpenZone will make sure that you don't
give it the name of an object that already exists.


{bmc images\delete.bmp} Deleting objects


Clicking  Edit\...Delete or pressing the 
Del key will delete any objects that you've selected. Since
OpenZone doesn't have an undo function, it will ask you for
confirmation before deleting them.


Changing an object's properties


The properties pane displays the properties of any single
object you've selected. You can alter an object's properties by
changing any of the settings. For scripted
objectsThe_scripted_objects_panel, changing the settings can have a
profound effect on the object.


{bmc images\MoveToCurrentPosition.bmp} Moving an object to your
location


A quick way to position an object exactly where you want it is to walk
to that location in fly mode and click Object\...Move to current
position. The object will instantly appear where you are
standing. This is most useful when the Walk along
groundGeneral_OpenZone_display_options option is checked.


{bmc images\MoveToCrosshair.bmp} Moving an object so it appears
in front of you


Similarly, an object can be moved so that it will sit at the create
object crosshairThe_mesh_library that can be set to appear in front of
you when in fly mode. Clicking  Object\...Move to create object
crosshair will make the object appear in the crosshair.


{bmc images\MoveToHotspot.bmp} Moving an object to a
hotspot


See the section on hotspotsAdding_object_placement_hotspots to learn
about hotspots and how you can use them.


{bmc images\MeshLibrary.bmp} Exporting your object to the mesh
library


This is covered under the mesh libraryThe_mesh_library.


{bmc images\Group.bmp}  Grouping multiple objects


Clicking  Object\...Group or clicking the toolbar button
will show a popup asking for the name of the new group object. All
selected objects will be placed in the group and their locations will be
changed relative to the location of the group. The new group object will
be given a location equal to the average of the object locations.


{bmc images\Ungroup.bmp} Ungrouping objects


This is the reverse of the group operation. Clicking
Object\...Ungroup will destroy the selected group object
and make all of the objects it contains separate objects again. Their
locations will be changed to absolute coordinates instead of coordinates
relative to the group.


{bmc images\Translate.bmp} Shifting objects by a specified
amount


Clicking  Edit\...Translate will display a popup window
where you can enter an X, Y, and Z value. Clicking Ok will
shift all selected objects by the amounts specified. The X axis
corresponds to the north-south axis with X increasing as one moves
north. The Y axis corresponds to the east-west axis with Y increasing as
one moves west, and the Z axis corresponds to the up-down axis where Z
increases as one moves up.


{bmc images\MoveNorth.bmp} {bmc images\MoveSouth.bmp} {bmc
images\MoveWest.bmp} {bmc images\MoveEast.bmp} {bmc
images\MoveUp.bmp} {bmc images\MoveDown.bmp} {bmc images\SizeUp.bmp}
{bmc images\SizeDown.bmp} Moving and sizing objects in small
increments


Below the main 3D viewer are eight buttons. The first six let you move
any selected objects in 0.1-unit increments for precise positioning.
They move the objects North, South, West, East, up, and down,
respectively. The last two buttons let you increase or decrease object
sizes by 10 percent for each click.



Manipulating mesh objects


Mesh objects are treated a bit separately and can be altered by special
operations that are available from the Mesh menu. Mesh
objects are objects that are purely formed by polygons, such as objects
that you might have importedImporting_objects_and_entire_zones. Objects
created with Dungeon Builder and imported, for example, are pure mesh
objects.


Pure mesh objects are objects like this:


- Objects you have importedImporting_objects_and_entire_zones,
such as .3DS files

- Objects resulting from splitting a mesh object with the
ground

- Objects resulting from splitting the ground at a certain
elevationThe_Ground_Editor

- Objects you've specifically turned into mesh objects by
clicking  Object\...Convert objects to meshes


That is, pure mesh objects are not scripted
objectsThe_scripted_objects_panel.


{bmc images\ConvertToMesh.bmp} Converting objects to pure mesh
objects


Clicking  Object\...Convert objects to meshes will turn
all selected objects into pure mesh objects. Pure mesh objects only have
location, rotation, and size as parameters and are nothing more than a
collection of polygons. However, OpenZone has operations available only
to mesh objects that allow some advanced effects. This is an
irreversible operation and should be used with extreme care.


{bmc images\PolygonProperties.bmp} Editing individual polygons
in mesh objects


For pure mesh objects, OpenZone has a feature where you can click on
individual polygons and change their properties. You can change their
textures, their opacity, or the ability of players to move through them.



To edit polygons, select them from Fly
modeLooking_at_your_zone_and_moving_around_in_it. Green selection
triangles will highlight the polygons. Then click  Mesh\...Change
polygon properties\..., or click the matching toolbar button. A
dialog will show that lets you change certain properties:


- The polygon's texture (or textures if you are using an
animated texture)

- The polygon's opacity map texture

- The animation time in seconds for one complete cycle when
using animated textures

- If the player can pass through the polygon

- If the polygon has a color applied to it

- If the polygon's texture is masked (used for 8-bit textures:
the mask color is color #0 in the color palette)

- If the polygon is opaque, semitransparent (50% alpha), or
fully transparent


To change a texture, delete the existing texture with the Delete
texture button, select a new texture with the drop-down or browse
button, and add it to the list with the Add texture
button. To also use a separate opacity map, select an opacity texture
before clicking Add texture. Animated textures are
specified by adding multiple textures to the list, such that textures
are displayed one at a time in the time amount specified. The other
options should be pretty self-explanatory, but it's worth noting that
the specific color applied to polygons cannot be changed at this
time.


If you have selected multiple polygons, the popup window will reflect
the properties of the first polygon selected, and when you click
Ok all of the selected polygons will be changed to match
the chosen settings.


{bmc images\DeletePolygons.bmp} Deleting polygons from a mesh
object


It's possible (though unlikely) that OpenZone might leave some
leftover polygons from your dungeon entrance polygons above the ground
after splitting an object with the ground (see below). Or, perhaps as a
result of splitting operations you've performed (see below), you might
want to delete certain polygons from a mesh object. You can get rid of
them by selecting the polygons and clicking Mesh\...Delete
polygon from mesh. Remember, you can select multiple polygons by
right-clicking.


{bmc images\SplitWithGround.bmp} Splitting a mesh object with
the ground


This is intended for dungeons created with Dungeon Builder. Its purpose
is to let you integrate dungeons with your OpenZone zones.


To use this feature, move your imported dungeon such that its entrances
just poke through the ground. Then select all of the objects that poke
through the ground (using Shift-click or Ctrl-click in the object list,
or by right-clicking on them directly) and click Mesh\...Split
with ground mesh. OpenZone will take the following steps:


- Any parts of the objects you've selected that poke through
the ground will be chopped so that they become flush with the ground
surface.

- The ground tiles affected will be hidden (see the Ground
EditorThe_Ground_Editor for details.)

- For each newly hidden ground tile, OpenZone will create a mesh
object beginning with "split" that looks the same but has a hole for
the entrance.


In this way you can make your dungeon entrances seamless with your
ground. If it doesn't look right, you can undo it with the following
steps:


- Delete the dungeon objects you imported.

- Delete the "split" objects OpenZone created.

- Go into the Ground Editor and unhide the ground
tiles by right-clicking on them.


{bmc images\SplitAlongGrid.bmp} Splitting selected mesh objects
along a grid


OpenZone can exportExporting_your_zone zones to verying formats,
including 3D Studio MAX (.3DS) format. Unfortunately, some formats have
limits on how many polygons objects may contain, and exceeding this
limit can at best cause an improper export. You can remedy this by
breaking objects up. If you select multiple mesh objects, clicking
Mesh\...Split selected meshes along grid will split the
objects into smaller ones. The only reason to use this feature is when
exporting to a format that has such polygon limitations, like
.3DS.


{bmc images\SplitMeshes.bmp} Splitting selected mesh objects
with each other


Mesh\...Split selected meshes with each other is a powerful
feature that can, for instance, be used to remove excess polygons or to
poke holes in mesh objects. If you have two or more mesh objects that
intersect each other, you can split their polygons at all places where
they intersect. For example, if a mesh object pokes through another mesh
object that represents a wall, this option will isolate the region in
the wall where they intersect (and will also isolate the region in the
object that pokes through it where they intersect). Deleting isolated
polygons created with this option could result in a specially-shaped
window, for instance.


{bmc images\CombineMeshes.bmp} Combining selected mesh objects
into a single mesh object


Mesh\...Combine selected meshes is useful after splitting meshes
with each other. For instance, if a split operation was used to create a
window, the result would be a wall mesh and a mesh for the inside of the
window. The two meshes could be combined into a single wall mesh with
this option. The resulting mesh will always start with the word
"combine" and have a number at the end (but can be renamed, of
course).


{bmc images\InvertMeshes.bmp} Turning mesh objects
inside-out


Mesh\...Invert selected meshes is useful when using a mesh to
poke a hole in another mesh. For instance, a cylinder could be used to
punch a hole in a wall to make a window, but the polygons of the
resulting windowsill would still point outward rather than inward. This
operation reverses the way all polygons in a mesh object point so they
can face the other way.


{bmc images\FlipTexCoords.bmp} Flipping textures on
polygons


Mesh\...Flip selected polygon texture coordinates is useful when
dealing with textures that, for instance, cover an entire wall section
or an entire door. It allows you to negate the texture coordinates of
all selected polygons so the texture (not the polygons)
appears to face the other way. An example of its use would be to make
the rear of a door line up with the front so that the door handle is on
the same side.


Scaling textures on polygons


Mesh\...Multiply selected polygon texture coordinates lets you
rescale textures on an object. For example, if a texture is appearing
too pixelized, you can scale the coordinates up.
 {style="margin-top:3pt;margin-bottom:3pt;"}
77(#_NDEF_77 "{$} Adding a light source to your zone"){#_NREF_77}78(#_NDEF_78 "{#} Adding_a_light_source_to_your_zone"){#_NREF_78}79(#_NDEF_79 "{K} Adding a light source to your zone"){#_NREF_79}80(#_NDEF_80 "{+} Using OpenZone:000"){#_NREF_80}}Adding
a light source to your zone 


{bmc images\AddLight.bmp} Adding a light source to your
zone


In fly mode, you can add a light source by clicking the toolbar button,
clicking  Object\...Insert light source, or pressing
Ctrl-L. A light source will be created just in front of
your position. You can get OpenZone to display the light by turning on
the Show zone light sourcesGeneral_OpenZone_display_options
option.
 {style="margin-top:3pt;margin-bottom:3pt;"}
81(#_NDEF_81 "{$} General OpenZone display options"){#_NREF_81}82(#_NDEF_82 "{#} General_OpenZone_display_options"){#_NREF_82}83(#_NDEF_83 "{K} General OpenZone display options"){#_NREF_83}84(#_NDEF_84 "{+} Using OpenZone:000"){#_NREF_84}}General
OpenZone display options 


{bmc images\DisplayOptions.bmp} General OpenZone display
options


Clicking the display options button on the right portion of the main
window brings up the  display options pane. Here you can
set certain options:


- The texture set for your zone

- Whether OpenZone should display transparent polygons

- If displaying transparent polygons, whether to show them as
solid rather than translucent

- Whether to place a light source at your position in fly mode,
as if you were carrying a torch

- Whether to show zone light sources

- Whether to show zonelines as transparent polygons

- Whether to force you to walk along the ground in fly
mode

- The ambient light level


Setting the texture set


This setting is actually saved with your zone, but its effect is purely
visual in nature. It contains a list of any subfolders found under your
library\textures folder. When this is set to anything
other than "(none)", all texture drop-downs in OpenZone will contain
any textures found in this folder as well as textures in the
library\textures folder. This allows you to place certain
zone-specific textures (like signs) in subfolders so they're only
visible in the list when you're working on that zone.


Showing transparent polygons


This is useful when placing objects in your zone and is on by default.
It lets you see where the zone bounds are in your zone in the main 3D
view, so you don't mistakenly place something where the player can't
reach.


Simulating a light source held by the player


Turning this on will cause a yellowish light source to follow you
around when you move around in fly
modeLooking_at_your_zone_and_moving_around_in_it. It's there to
simulate torchlight and serves no other purpose. It's important to make
sure the ambient light setting isn't at maximum or this
won't have any effect (bright sunlight tends to drown out
torchlight).


Showing light sources you've added to your zone


If you have this checked and have the ambient light level
set below the maximum, OpenZone will attempt to display the light for
you. Note though that this depends greatly on your video card's OpenGL
implementation. The default OpenGL 1.1 specification only allows for up
to eight light sources and OpenZone uses two of them for the ambient
light and player's light, respectively. At any rate OpenZone will use
as many light sources as your video card's OpenGL implementation will
allow.


Showing model origins you've added to your zone


Model origins aren't actual objects but represent where the (0,0,0)
point should be for item objects you intend to export to the mesh
libraryThe_mesh_library (e.g. swords, shields, helmets, and any other
items that would appear on a player or creature). They show up as blue
spheres, and there is a drop-down that determines how they should
appear.


Showing hot spots you've explicitly added to your zone or are
contained in objects you've placed


Hotspots aren't actual objects but are locations that you can use to
quickly place objects, such as for placing
doorsAdding_object_placement_hotspots. They show up as either red or
orange spheres (where orange is the one that's selected), and there is
a drop-down that determines how they should appear.


Showing where your zonelines are


If you've added zonelines with the Ground EditorThe_Ground_Editor, you
can see where they are in the main 3D viewer by checking this option.
OpenZone will show zonelines as translucent polygons with a slight
purplish cast.


Forcing you to move along the ground


This option is for  fly mode. When checked, no matter
what you do OpenZone will keep you fixed to the ground as you movre
around (no moving up or down). There are a couple things to note about
this mode:


- When using standard OpenZone ground meshes, it only looks at
the actual ground mesh.


This means that if you place a staircase object, for instance, you
can't walk up the stairs. This option only looks at the
ground, and nothing else.


- When using imported zones, all polygons in the
zone are taken into account.


This is because, in the absence of a standard OpenZone ground mesh,
OpenZone has no way of knowing which parts of your zone are ground and
which aren't. It therefore looks at the entire zone as ground. However,
it is smart enough in that it won't look at polygons that face down.
This means that if you import a zone from Dungeon Builder, for instance,
you'll walk along the floors of your dungeon since the ceiling polygons
will face down and OpenZone will therefore ignore them.


Setting the ambient light level


The default light level is 100% white, but you might want to see how
your zone looks at night. You can lower the light level with the slider
to something more acceptable.
 {style="margin-top:3pt;margin-bottom:3pt;"}
85(#_NDEF_85 "{$} Exporting your zone"){#_NREF_85}86(#_NDEF_86 "{#} Exporting_your_zone"){#_NREF_86}87(#_NDEF_87 "{K} Exporting your zone"){#_NREF_87}88(#_NDEF_88 "{+} Using OpenZone:000"){#_NREF_88}}Exporting
your zone 


{bmc images\ExportToS3D.bmp} Exporting your zone for play


To export a zone for play, click File\...Export\...Export to
.S3D. OpenZone will ask you for the short name of your zone (the
prefix to use for the files, e.g. "deadgulch"). If you've set the
zone's short nameEditing_overall_zone_properties in its properties the
prompt will automatically be filled in. Once you've given the name to
use, click Ok. If the zone already exists, you'll be
prompted for confirmation.


One final window will appear warning you to make sure that everything
you've included must be free for your use. While, strictly speaking,
OpenZone isn't responsible for what you do with it, I don't want
improper use of other people's property reflecting badly on OpenZone,
so please, make sure you have permission to use everything you place
into your zones.


Once you've clicked Ok, sit back and let OpenZone export
the zones, which can take a few minutes. When it is finished you should
see three .S3D files in your zones folder, where each one
begins with the short name you provided. OpenZone will also create a
file with extension .SQL that contains SQL statements for door placement
(see below).


{bmc images\ExportZoneObjects.bmp} Creating an .S3D file that
only contains objects


Clicking  File\...Export\...Export zone objects to .S3D
will let you create an .S3D file that only contains the zone objects
that you have added in the Zone Properties
PopupEditing_overall_zone_properties. Unlike exporting your entire zone,
however, this option prompts you for the name of the .S3D/WLD files to
create. This lets you call the output file anything you wish; for
example, entering "global_chr" in the prompt will create a file called
"global_chr.s3d" that contains the file "global_chr.wld". This lets
you create specialized files that only contain objects and aren't part
of normal zone files. This is especially useful for creating equipment
files, e.g. weapons, shields, etc.


{bmc images\ExportToSQL.bmp} Creating an .SQL file that only
contains door placement SQL statements


Clicking  File\...Export\...Export to .SQL will create a
text file with extension .SQL in the zones folder that
conatins SQL statements for door placement. The first line will erase
all doors for your zone and the rest will set the door locations. You
can use this file with your database to quickly set all door locations
in your zone.


Exporting just the main .WLD file of your zone


This is only useful if you just want to take your zone and do something
else with it, like run it through a map making program. Clicking
File\...Export\...Export to .WLD will export only the .WLD
file for your zone. Operation is similar to exporting to .S3D as
described above.


Exporting zone line information


Clicking  File\...Export\...Export zone line info to .ZPT
will create a text file containing information for all of the zone lines
you've placed in the Ground EditorThe_Ground_Editor. This is intended
for use with the EQEmu database and you should be able to enter this
information directly into the appropriate fields.


Generating a .MAP file for use with ShowEQ or the EQEmu Admin
Tool


It's first worth noting that the .MAP file this creates is
not the same as the .MAP file used by EQEmu for
line-of-sight calculations. That is just an unfortunate coincidence of
having the same extension. Clicking File\...Export\...Generate
ShowEQ/Admin Tool .MAP file pops up a window where you can
generate an overhead map for use by these two tools. If you have already
set the zone's long name and short name they will be set here,
otherwise you'll have to enter them.


When ready, click  Generate Map. This may take a while to
complete, but when it is finished you should see a rough overhead map of
your zone above. Click Ok afterward to save the .MAP
file.


{bmc images\Import3DS.bmp} Exporting your zone to a standard 3D
Studio Max file


Clicking  File\...Export\...Export zone objects to .3DS
will create a standard 3D Studio Max file containing your zone and all
placeable objects. With this option you can use OpenZone for any
project, since there are counteless tools that can read .3DS files
(including 3D Studio Max itself). For instance, you can use OpenZone to
quickly mock up a zone and then touch it up in a more advanced tool, or
you could just use OpenZone exclusively for your zone building and
export when done.


{bmc images\ExportToXWF.bmp} Exporting your zone to an
eXtensible World Format (.XWF) file


The eXtensible World Format is a new format that I helped design that
is, by it's name, easily extensible. It uses the atom concept where
atoms can contain other atoms, and each atom type is identified by a
fourcc (four-character-code) string. The current .XWF specification is
located here (external web
link)!ExecFile(http://home.archshadow.com/\~daeken/xwf.html).


{bmc images\ExportToXWA.bmp} Playing with SimpleClient:
Exporting your zone to an eXtensible World Archive (.XWA) file


The eXtensible World Archive is to .XWF files as .S3D archives are to
the .WLD files and images they contain. It's an archive that is used to
package world files as well as any images they reference. It can also be
used to contain sounds, or be put to any other use that .S3D files are.
The new client  SimpleClient uses .XWA files, and expects
to find .XWF files inside them. When making zones for use with
SimpleClient, you should export to .XWA rather than .S3D.
 {style="margin-top:3pt;margin-bottom:3pt;"}
89(#_NDEF_89 "{$} Importing objects and entire zones"){#_NREF_89}90(#_NDEF_90 "{#} Importing_objects_and_entire_zones"){#_NREF_90}91(#_NDEF_91 "{K} Importing objects and entire zones"){#_NREF_91}92(#_NDEF_92 "{+} Using OpenZone:000"){#_NREF_92}}Importing
objects and entire zones 


Importing objects and entire zones


OpenZone is capable of importing three kinds of files for importing
objects into your zone:


- 3D Studio MAX (.3DS) files

- OGRE-XML .MESH files

- Quake 3 .BSP files


None of these import perfectly, but OpenZone does a fair job at
importing .3DS and Quake 3 files. OpenZone will keep whatever is in your
zone when it imports: in this way you can, for instance, make an outdoor
zone and then import a Dungeon Builder .3DS file to add dungeons and
tunnels.


{bmc images\Import3DS.bmp} Importing 3D Studio MAX files into
your zone


There are two ways to import .3DS files into your zone: a straight
import and an import as a ground mesh. The first one simply imports the
objects as they are stored in the .3DS file and should be used for
importing Dungeon Builder zones. The second method is for importing .3DS
files that only contain ground areas where you want OpenZone to convert
them to standard OpenZone ground meshes. It's worth noting that the
second method will probably result in a loss of detail since OpenZone
ground meshes use a grid system.


To import a .3DS file into your zone, click File\...Import\...3D
Studio MAX (.3DS). You will be prompted for the .3DS file to
import. It is vital that any textures it references already be in your
 library\textures folder.


OpenZone is capable of reading .BMP, .JPG, and .TGA textures.


If any of the objects you have in your .3DS file start with
"oz_water", "oz_lava", or "oz_pvp", OpenZone will take them to
mean that you are defining water, lava, or PvP areas instead of actual
objects. In the case of water or lava areas, the first texture that
OpenZone detects in those objects will be used for the surface texture.
Such areas will be classified as "irregular", since they don't
necessarily have to be rectangular in shape. The only requirement is
that the sides of the areas you define be vertical (not sloped) and that
the top and bottom be flat.


Importing a .3DS file as a ground mesh is similar and is accomplished
by clicking  File\...Import\...3D Studio MAX (.3DS) as ground
mesh. The .3DS file will be imported and converted to a standard
ground mesh that you can edit with the Ground
EditorThe_Ground_Editor.


{bmc images\Import3DSToMesh.bmp} Importing 3D Studio MAX files
into the mesh library


When creating objects for use as equipment objects, it's important
that the X-Y-Z origin information be preserved. Importing these objects
into your zone and then exporting them into the mesh
libraryThe_mesh_library destroys this information, so OpenZone contains
a separate menu entry specifically for importing objects directly into
the mesh library. This ensures that the X-Y-Z origin information is
preserved, and has other additional benefits. Using this option will
leave your zone unaffected; it only changes the mesh library.


To import a .3DS file directly into OpenZone's mesh library, click
 File\...Import\...3D Studio MAX (.3DS) into the mesh
library. You will be presented with a standard file open dialog,
with the added feature that multiple files may be selected. In this way
you can import an entire batch of .3DS files in one operation.


If only one .3DS file is selected for import then you will be presented
with a dialog prompting for the name of the object as it will be
presented in the mesh library. Otherwise, the names will be taken from
the file names (e.g. oak_tree.3ds would be imported as
"oak_tree").


If OpenZone detects that some objects conform to the standard equipment
naming convention (itxxxx, where xxxx is a number) it will prompt you to
ask if it should modify the names to ensure they are fully compliant. A
fully-compliant equipment item name is ITxxxx_DMSPRITEDEF, where xxxx is
a number. If you click "Yes" to this prompt, OpenZone will modify the
names for any files that fit this form before importing them (OpenZone
is smart enough to not mangle any names that are already
fully-compliant). In any case, the original .3DS files will remain
unaffected.


After all files have been imported and converted, the mesh library
listing will be updated to include the new objects.


Importing OGRE-MESH .XML files


You can import one of these files by clicking
File\...Import\...Ogre XML-Mesh. You will be prompted for
the file to load, and, like importing .3DS files, should already have
the textures it references in your library\textures
folder.


{bmc images\ImportQuake.bmp} Importing Quake 3 .BSP files


Importing these files is a bit more complex than importing the other
types of files, since Quake 3 maps use a directory structure to store
textures and other information. OpenZone requires that you create a
library\textures\quake3 folder in this case and that you
extract all Quake 3 .PK3 files that you will need into this folder. The
actual .BSP file doesn't have to be here, but everything it uses will
have to be there. As the .BSP file is loaded, OpenZone will extract
whatever information it needs from this folder.


It's worth noting that the Quake 3 engine does a lot more than
OpenZone and the standard client support and so importing will probably
never be perfect. Quake 3 shaders are especially hard to import and
imperfections should be expected wherever they're used.


{bmc images\ExportToXWF.bmp} Importing eXtensible World Format
(.XWF) files


The eXtensible World Format is a new format that I helped design that
is, by it's name, easily extensible. It uses the atom concept where
atoms can contain other atoms, and each atom type is identified by a
fourcc (four-character-code) string. The current .XWF specification is
located here (external web
link)!ExecFile(http://home.archshadow.com/\~daeken/xwf.html).


{bmc images\ImportAN8.bmp} {bmc images\ImportAN8_Library.bmp}
Importing Anim8or files


Anim8or (available at
http://www.anim8or.com!ExecFile(http://www.anim8or.com)) is a free
program for making 3D models. Like .3DS files, OpenZone can import .AN8
files either into your zone or directly into the mesh library. Clicking
File\...Import\...Anim8or file (.AN8) will import an
Anim8or file into your zone, and clicking 
File\...Import\...Anim8or file (.AN8) into the mesh library will
import an Anim8or file as a mesh library object. At this time, however,
OpenZone's importer can't handle grouped meshes in Anim8or files so
it's important to first make sure that any meshes in your Anim8or files
are ungrouped.
 {style="margin-top:3pt;margin-bottom:3pt;"}
93(#_NDEF_93 "{$} Adding an OpenZone scene to your existing zone"){#_NREF_93}94(#_NDEF_94 "{#} Adding_an_OpenZone_scene_to_your_existing_zone"){#_NREF_94}95(#_NDEF_95 "{K} Adding an OpenZone scene to your existing zone"){#_NREF_95}96(#_NDEF_96 "{+} Using OpenZone:000"){#_NREF_96}}Adding
an OpenZone scene to your existing zone 


{bmc images\InsertScene.bmp} Adding an OpenZone scene to your
existing zone


There are times when it might be useful to copy in a bunch of objects
from one OpenZone scene to another. For example, let's say you created
a zone with this really neat tower, but you decide that you want the
tower to be used in another zone instead. Or, perhaps you want the same
tower to be available in more than one zone. Perhaps the tower contains
too many polygons to go into the mesh library, since mesh library
objects are limited to about 1,500 polygons. There is an easy way to
import parts of an existing zone into another zone.


The way to do this is first to make a copy of the zone containing the
object(s) you want, delete everything in the copied zone that you don't
want to copy, and then import the zone into another one. We will use an
example to illustrate how this works.


Let's say you have two zones, "magicdesert" and "hauntedplain".
Zone "magicdesert" contains the really cool tower you created, and you
also want to use it in "hauntedplain". The steps to do this are:


- Open "magicdesert" and save it as "tower".

- Now that you are working with "tower", delete everything
except the tower object (including deleting the land meshes). Don't
forget to go into the Water EditorAdding_water,\_lava_or_PvP_areas and
get rid of any water, lava, or PvP areas.

- Save your zone ("tower").

- Open "hauntedplain".

- Click Edit\...Insert scene\... and open
"tower". Everything in the zone will be imported, and the tower will
be placed at the exact location it was at in the original
"magicdesert" zone (since you didn't move it when you were working in
the "tower" zone).

- Move the tower object to where you want it.

- Save the "hauntedplain" zone.


In this way you can take any objects you create in one zone and easily
re-use them in another.
 {style="margin-top:3pt;margin-bottom:3pt;"}
97(#_NDEF_97 "{$} Creating and exporting creatures with Anim8or"){#_NREF_97}98(#_NDEF_98 "{#} Creating_and_exporting_creatures_with_Anim8or"){#_NREF_98}99(#_NDEF_99 "{K} Creating and exporting creatures with Anim8or"){#_NREF_99}100(#_NDEF_100 "{+} Using OpenZone:000"){#_NREF_100}}Creating
and exporting creatures with Anim8or 


{bmc images\ImportAN8.bmp} Creating and exporting creatures
with Anim8or


Anim8or (available at
http://www.anim8or.com!ExecFile(http://www.anim8or.com)) is a free
program for making 3D models. It can make static models that OpenZone
can importImporting_objects_and_entire_zones, and, it can make animated
creature models using skeletal animation. If you have Anim8or files that
conform to certain standards, you can use OpenZone to export them as
usable creatures.


Creating creatures is an involved process and is explained below as a
series of steps:


1. Making an Anim8or file that conforms to OpenZone's standards

2. Adding the Anim8or file to OpenZone's creature library

3. Adding creatures to your zone

4. Exporting your zone including creatures


OpenZone comes with a sample Anim8or creature file in its
library\creatures folder that you can use to learn how to
make creatures and use as a template for more. In fact, since it comes
with a full set of humanoid animations, copying the file and modifying
it as necessary is highly recommended.


Step 1: Making an Anim8or file that conforms to OpenZone's
standards


To use an Anim8or creature file, the file needs to conform to certain
standards so OpenZone can properly export it. These standards can be
broken into several categories: object standards, figure standards,
sequence standards, material standards, and texture standards.


Anim8or object standards


The first object standard concerns the names of objects. Creature
models must have names of the form
bodyaabb\_headccdd, where:


- aa is the number of the body mesh type. aa should always be 00
for normal body types, and 01 for robed variants.

- like aa, cc is the head mesh type. For player models, it
should range form 00 to 03, corresponding to bare, leather, chain, or
plate, respetively. For non-player models, it should begin at 00 and
migrate to 01, 02, etc. for each separate head mesh type (e.g. 00 could
be a cyclops head, 01 could be a hill giant head, etc.)

- bb and dd contain the skin number, starting with 00. For
player models, they should contain 00, 01, 02, or 03 for bare, leather,
chain, or plate armor skins, respectively. They are also special in that
they are related to mesh types. For instance, if the body mesh type is
01 (robed), then the body skins should range from 10 to 16. For heads,
dd should also track alternate meshes, so if you have a head mesh 01
then the head skin should also be 01 so it uses the correct skin
set.


Anim8or objects are comprised of a series of meshes. Anim8or allows
them to be grouped, but OpenZone's Anim8or importer doesn't understand
mesh groups. It's important to make sure that no meshes are grouped
together in your Anim8or files or they won't import properly.


Object meshes can have names, too, and for creature models it's
strongly recommended that the meshes have the same names as the bones
that control them (e.g. calling the head mesh "he", which is also the
same name as the head bone). When exporting a creature, OpenZone needs
to determine which bone most controls the points in the mesh and doing
this works best. If OpenZone can't find a matching bone for a mesh it
will attempt to determine the best bone by using Anim8or's bone
influence settings, but this is untested and unsupported.


Head meshes are special in that they must be named "he". The reason
is that head meshes are handled separately from body meshes and OpenZone
needs to be able to determine which mesh in your model is the head. This
is only necessary, however, if you are using alternate head meshes for
your creature (e.g. a snake model might not need alternate heads).


Anim8or figure standards


Because an Anim8or creature file can contain multiple objects (e.g. one
for each armor type), they must all be attached to your Anim8or figure.
When in Anim8or in figure mode, use the entries under the Build menu to
make sure that all of your objects are attached to the figure.


Anim8or sequence standards


Sequence names must also conform to naming standards. Their names must
be of the form name_cycleid_frametime_id, where:


- name is the sequence name and can contain anything you
wish

- cycleid is either N or C. C means that the sequence is cyclic
(that is, it repeats), like walking. N means it's not cyclic, like
kneeling. The reason for the distinction is that, for cyclic animations,
the last frame in the sequence should be identical to the first. When
exporting cyclic sequences, OpenZone will know to discard the last frame
so there are no hitches in the animation.

- frametime contains the time in milliseconds (ms) for one
animation frame.

- id contains the game-specific animation id. They are all of
the form abb, where a is one of the letters (C, D, L, O, P, S, T) and bb
is a number starting with 01. The letter-number combination identifies
specific animations, for example P01 is a standard walking/moving
animation.


Anim8or material standards


The material standards only apply to creating player models. Player
models support up to eight facial textures. To specify alternate faces,
the material names in the Anim8or file should be called face01, face02,
etc., up to face07 (face00 is implied as it would be used in your
objects).


Anim8or texture standards


Another standard has to do with texture filenames. For head and body
textures, they should be of the form aaabbccde.bmp, where:


- aaa is a model id, like "elm". It doesn't have to match the
race identifier used when exporting your creature in OpenZone.

- bb contains a body part reference, e.g. "he" for head,
"ft" for foot, etc.

- cc contains a skin reference. For example, for player models
it should contain 00, 01, 02, or 03 for bare, leather, chain, and plate,
respectively. For non-player models it should be numbered starting with
00, where each number denotes a different skin (e.g. 00 for one tribe,
01 for another, 02 for yet another, etc.)

- d refers to alternate facial skins for player models, e.g. 0
through 7. For all other textures it should contain 0.

- e is a numerical identifier for different textures used on a
model part. For instance, if three different textures are used on the
head, it should be numbered 0 through 2.


The reason for this standard is because exported creature files don't
contain information linking alternate skins together. Rather, the
texture filenames are directly used, where the cc value is used to
identify skin variants.


Step 2: Adding the Anim8or file to OpenZone's creature library


To use an Anim8or creature file with OpenZone, copy the Anim8or file
and all textures it uses to OpenZone's library\creatures
folder. Do not copy creature textures to the
library\textures folder. OpenZone deliberately treats
creatures differently than zones to make it easier to manage textures,
as it would be extremely rare to use a creature texture for standard
zone objects.


Anim8or files reference the full path of textures, but you can ignore
that detail \-- OpenZone is smart enough to strip off the path of the
textures that Anim8or files reference. As long as the creature textures
are in the library\creatures folder, OpenZone will find
them.


Step 3: Adding creatures to your zone


To add creature models to a zone, open the zone properties
windowEditing_overall_zone_properties and add creature models to your
zone. When finished, saving the zone will also save the list of
creatures that were added, so that reopening the zone at a later time
will restore the creature list.


Step 4: Exporting your zone including creatures


Exporting a zone that contains creatures is no different than exporting
a zone that doesn't contain any creatures. At the appropriate time,
when OpenZone creates the zone files it will load each Anim8or file and
export the contents (at this time, creatures can only be exported to
.S3D format). It's worth noting that if your zone contains many
creature models it can take some time to import and export each one.
Because it can take a while to import an Anim8or file, OpenZone doesn't
import them when it starts up, but rather waits until it's time to
export them as part of a zone.
 {style="margin-top:3pt;margin-bottom:3pt;"}
101(#_NDEF_101 "{$} Adding object placement hotspots"){#_NREF_101}102(#_NDEF_102 "{#} Adding_object_placement_hotspots"){#_NREF_102}103(#_NDEF_103 "{K} Adding object placement hotspots"){#_NREF_103}104(#_NDEF_104 "{+} Using OpenZone:000"){#_NREF_104}}Adding
object placement hotspots 


{bmc images\InsertHotspot.bmp} Adding object placement
hotspots


When creating objects (such as houses or huts, for example), OpenZone
allows you to add hotspots to them. Hotspots aren't visual objects in
and of themselves, but they make it easy to place objects precisely. For
instance, if you create a hut object that you wish to add to the mesh
libraryThe_mesh_library, before exporting the object you could add a
hotspot on either side of the doorway. Then, when copies of the object
are placed into a zone, you could easily place a door object at one of
the hotspots. When many such huts are in a zone, it makes the job of
accurately placing doors much easier.


To add a hotspot, click Object\...Insert hotspot. A
hotspot object will be created just as any other object. Hotspots
display as small transparent spheres much like model origin objects, and
the display optionsGeneral_OpenZone_display_options panel has a setting
for how they show up.


To make sure an object you want to export to the mesh library has its
hotspots, make sure they are grouped with the object and export the
entire group (just as you would do if you were making something like a
torch and wanted to ensure the light object was also exported).


{bmc images\SelectHotspot.bmp} Selecting hotspots


Selecting an object will cause OpenZone to display all hotspots inside
it, where the selected hotspot will display in orange and all others
will display in red. If a hotspot explicitly exists in your zone you can
simply select it in the object tree. If the hotspots are
contained in an object that was placed from the mesh library (and
therefore its component pieces can't be individually selected from the
object tree), you can select individual hotspots by clicking
Object\...Select hotspot\..., which will pop up a window
where a hotspot can be selected.


{bmc images\MoveToHotspot.bmp} Moving objects to hotspots


To move an object to a hotspot, click Object\...Move to last
selected hotspot. This only works on one object at a time, and
the object will instantly move to that point. If the object had been
placed from the mesh library, its Gravity setting will automatically be
set to False.


It's worth noting that hotspots can be used for more than doors. They
can be used for placing furniture, torches, or anything else you can
think of. The only thing that sets doors apart is that they should have
their DoorRef property set to True so that they will be
exported as SQL statementsExporting_your_zone rather than explicitly
placed into the zone.
 {style="margin-top:3pt;margin-bottom:3pt;"}
105(#_NDEF_105 "{$} The scripted objects panel"){#_NREF_105}106(#_NDEF_106 "{#} The_scripted_objects_panel"){#_NREF_106}107(#_NDEF_107 "{K} The scripted objects panel"){#_NREF_107}108(#_NDEF_108 "{+} Scripted objects:000"){#_NREF_108}}The
scripted objects panel 


The scripted objects panel


Right below the main toolbar is a series of tabs with icons below them.
Each tab is a script category, and each category contains a series of
buttons. These buttons correspond to scripted objects that OpenZone can
place.


So\...what is a scripted object?


If you look in the folder where you installed OpenZone, you should see
a library subfolder. Under that, there should be a folder
called scripts, and in there should be a number of .SCP
and .BMP files. The .SCP files are the scripts themselves and the .BMP
files contain the icons that you see on the screen.


Scripted objects, put simply, are small programs that create
three-dimensional objects when OpenZone runs them. A script might be
designed to create a staircase, a column, a chair, or anything that you
code it to do. The central concept behind scripts is
flexibility: with a well-written script, you can have a
single script that can create lots of different variations of that type
of object.


Lets say you want to place some staircases, but you need staircases of
varying heights and numbers of steps (maybe one that's 10 units tall
over here, and one that's 15 units tall over there, each with a
different number of steps). Maybe you want to use different textures for
them, too, like wood for one and stone for the other. Normally you'd
have to create each object individually. With scripts, you can place a
single scripted object for each and set the parameters to
customize each staircase in the way you want.


That's the real power behind scripts: they define parameters and
create objects on the fly based on those parameters. The more flexible
the script, the greater the variation of objects you can create with
it.


You might have noticed that there are categories of
scripts, where each tab is a category. One of the commands in the
scripting language allows scripts to tell OpenZone the category in which
they belong (e.g. a wall script knows that it should be in the
"buildings" category). When OpenZone starts up it grabs all of the
scripts in the  library\scripts folder and creates
whatever category tabs it needs to store them.


Placing a scripted object


In fly modeLooking_at_your_zone_and_moving_around_in_it, move to where
you want to place the object. Then, choose the category containing the
script you're interested in, and then click on the appropriate script
button. OpenZone will prompt you for the name of the new object. Once
you enter a name and click Ok, OpenZone will create the
object in front of you.


Customizing a scripted object


Scripted objects' properties show up in the Properties
pane under the heading "Properties". The exact nature of the
properties depends on the script itself, though most should be pretty
self-explanatory. OpenZone's script runner is pretty robust, so when
first starting out feel free to play with the properties until you learn
what they do.


Default property values


One of the first things you should notice about your scripted objects
is that some properties have values in parentheses (such values also
show up in bright blue.) These represent default values
that the script defines.


As mentioned above, scripts define properties that you can set.
However, they also allow you to not set a property and in
that case scripts can define what to use if the parameter isn't set.
For example, take the lowly box scipt (under the "geometry" category).
It has parameters called "tex1" through "tex6" that define the
textures to use for each of its faces. However, if you only set
"tex1", "tex2" through "tex6" will change to contain what
you just set for "tex1", but they will be in parentheses. The reason
is that the box script defines the default values for these parameters
as the value given for "tex1" if they haven't been set.


Stop and take a deep breath. The reason behind this is twofold: First,
it lets the scripts define values that are either safe or reasonable
defaults. Second, they are there to reduce unnecessary work. If you
wanted to create a box that is all one texture, you only have to set the
first one with this method rather than all of them. The short of it is
that default values are very useful. You don't have to worry too much
about them if you don't plan on writing scripts, just know that when
you see a value in bright blue and in parentheses it means that the
script is using a default value because you haven't set it.


"fulltexture", "texscalex", and "texscalez" parameters


Whereas most scripted object parameters are pretty self-explanatory,
there are three very common parameters that aren't all that obvious.
They aren't necessarily in every script and there is no requirement
whatsoever to include them, but they come with many of OpenZone's
scripts and deserve special explanation.


OpenZone by default will attempt to come up with a reasonable way of
placing textures on scripted objects, but sometimes the result isn't
very desirable. The "fulltexture", "texscalex", and "texscalez"
parameters work together to give you greater control over how textures
are applied to the object.


When "fulltexture" is true, the "texscalex" and "texscalez"
parameters affect texture scaling on the object (when it's false
OpenZone uses its default texture painting algorithm). You can then use
the two scaling factors to change how your texture looks on the object.
They're useful for tweaking the level of texture detail you want the
object to show. The "texscalex" parameter works in the horizontal
direction of the texture and "texscalez" works in the vertical
direction.


Texture parameters


Texture parameters show up as drop-downs in the Properties
pane, with a small button containing an ellipsis ("\...") next
to it. You can use the drop-down to directly select a texture or click
the ellipsis button to set an animated texture.


If you click the ellipsis button to set an animated texture, a window
will pop up with a texture list at the top and a texture drop-down below
it. You can add textures to the list by choosing it in the drop-down (or
clicking the Choose\... button) and clicking Add
texture. Likewise, you can remove a texture from the list by
selecting it and clicking Delete texture. In this way you
can specify a set of textures that will form a single animated texture
when your zone is exported. Click  Ok to save your changes
or  Cancel to discard them.
 {style="margin-top:3pt;margin-bottom:3pt;"}
109(#_NDEF_109 "{$} WindScript: the scripting language"){#_NREF_109}110(#_NDEF_110 "{#} WindScript:_the_scripting_language"){#_NREF_110}111(#_NDEF_111 "{K} WindScript: the scripting language"){#_NREF_111}112(#_NDEF_112 "{+} Scripted objects_WindScript:000"){#_NREF_112}}WindScript:
the scripting language 


WindScript: the scripting language


OpenZone's real power comes from its ability to place scripted
objectsThe_scripted_objects_panel. When the program starts up, it
immediately loads all scripts it can find in the
library\scripts subfolder and precompiles them. If there
is any problem with a particular script, it logs it for you so you can
diagnose what's wrong. You can look at the script parsing log by
clicking View\...View script log from the main menu. If
OpenZone was able to parse the scripts with no problems, the log will be
empty.


It isn't necessary to learn the scripting language to build zones with
OpenZone, but learning it will allow you to use the program to its
fullest potential.


Note: The scripting language, like everything else in OpenZone, is case
insensitive. The amount of whitespace isn't important
either.


Important: WindScript statements have to all be on one line
(with one exception, see below). You can't begin a statement and end it
on a different line. I know this can be a pain when dealing with long
parameter lists, but you can define variables to make life
easier.


Basic parts of a WindScript script


There are several parts to a script:


- Script category

- Script parameters

- Variable declarations

- Main code

- Comments


The script category


Evey script should contain a line that begins with
category. This line defines where in the tabbed chooser
the script appears. Is it furniture? Did you script an entire hut and
therefore it's a building? Where do you want it to appear on the script
palette? OpenZone creates category tabs on the script palette for all
scripts it finds and places each script under its corresponding tab. If
the  category line isn't there, OpenZone will create a
"default" category and put your script there.


There is a special category called "other" you can also use to
prevent a script from showing up. Why would you want to do this? You
see, scripts can call other scripts, and you might want to make small
"helper" scripts that don't represent much in the way of placeable
ojects but are otherwise helpful. I'm not sure how useful this feature
is, but it's there just in case.


Script parameters


The whole point of scripting 3D objects is so you can easily customize
them. You do that by defining parameters the script takes that can be
set from the properties pane.


Here is an example set of parameters:


PARAM NumSteps Integer Default 10 ' Number of steps

PARAM Sides Boolean Default True

PARAM Back Boolean Default True

PARAM Tex1 String

PARAM Tex2 String Default Tex1


We've defined five parameters. The first one is an integer (with a
comment at the end), the next two are boolean (true/false) parameters,
and the last two are strings. But that's only scratching the
surface.


Four of them have default values. This means that, if you don't set
the parameter, they'll be preset to some value. This is important, from
several standpoints:


- First, the script has to have a starting point if you don't
set something. "Garbage in, garbage out" is the old saw. This is a way
of preventing garbage.


- OpenZone allows you to leave a parameter unset.
What if you decide you want to enhance a script after you've used it?
What if you add another parameter? Any scenes made before you did that
won't have that parameter set, so what do you do? Specifying default
values is a way to deal with this: it lets you start from a known state
with something you decide is reasonable.


- Look closely at the list of parameters above. Parameter
Tex2's default setting is Tex1! This is very useful: this way, you can
set a parameter only once and have it apply globally, but if you want to
specialize certain parts of your object, you can set other pieces as
well. It lets you make scripts that are very simple, since you don't
have to include logic that tests whether something has been set.


You can define five types of parameters: integer,
float, boolean, string, and
 script. OpenZone will automatically take "string" to
mean a texture, and in the properties pane it will
automatically present you with a drop-down for the string value that
lists all textures in your  library\textures folder (and
your textureset folder if you have that set too).
Likewise, boolean parameters result in a drop-down picker since the
allowable values are either "default", "true", or "false". In both
cases, they will have a topmost entry saying "(use default value)".
The default value has the effect of "unsetting" the value and causing
it to use the default. It's worth noting that you can always type in
any text you want for string parameters, which is useful for certain
scripts (such as the Door_ParmsThe_mesh_library script, where the string
is really text to be entered into a database).


The default value for a string parameter that doesn't have a default
set in the script is an empty string. When dealing with integer and
floating-point parameters in the properties pane, if you
clear the entry field and press Enter it unsets the value, reverting it
to the default. If there is no default in the script, OpenZone uses
zero.


When your scripted object is using a default value, the value in the
properties pane always shows up in bright
blue and in parentheses.


Default settings can also be expressionsScripting_language_expressions.
Parameter declarations like these are all legal:


PARAM bool_a Boolean DEFAULT false

PARAM bool_b Boolean DEFAULT false

PARAM bool_c Boolean DEFAULT bool_a and bool_b

PARAM int_i Float DEFAULT 0

PARAM int_j Float DEFAULT 0

PARAM int_k Float DEFAULT i\*4\*sin(j)

PARAM int_l Integer DEFAULT trunc(k)

PARAM dest_zone String DEFAULT "NONE"


Note that to specify a literal string you need to enclose it in double
quotes. You can only do this for default parameters or for SQLPARM
commands (see below).


Format of a parameter declaration


In general, a parameter declaration looks like this:


PARAM paramname paramtype \DEFAULT 
defaultvalue\ \READONLY\ \HIDDEN\


The default setting has already been explained above. The readonly
setting means that the parameter will display on the properties
pane, but you can't alter it. It sounds useless, but actually
isn't. This feature lets the script calculate useful information from
the parameters and present it back to the user. For instance, it might
be useful for the user to know the angle defined by X and Y values that
were entered as parameters. The script could define a readonly parameter
called xy_angle and define it like this:


PARAM X Float

PARAM Y Float

PARAM xy_angle Float Default X\@Y Readonly


The at sign (@) calculates the angle defined by two numbers, with the X
value coming before it 

and the Y value coming after it.


The hidden setting lets you define a parameter that won't appear on
the properties pane. This also sounds useless, but like the readonly
setting, actually isn't. It lets you use parameters as variables.


"Why use parameters as variables?", you might ask, "since we can
already define variables?" The answer is that parameters can contain
default values for other parameters. A real-world example is the
connected_corridor script. It lets users enter a corridor's endpoints
and the position along the corridor where there can be an intersection,
in terms of a percentage of the corridor's length. In return, the
script calculates the actual location of the intersection and presents
this back to the user as readonly parameters. This is a complicated
calculation and is made easier by using hidden parameters as variables
to make it simpler.  Variables can't be used as default values
for parameters, only other parameters, and this is why the hidden
setting exists.


Variable declarations


You have to pre-declare all variables before you use
them. I like to do it before any main code, but it isn't strictly
necessary. The following kinds of variable declarations are
supported:


VARIABLE varname boolean
\booleanvalue\

VARIABLE varname float \floatvalue\

VARIABLE varname integer
\integervalue\


It's pretty simple. You can call your variables anything you wish, as
long as they aren't reserved words (basically any WindScript keyword)
Variables can be either, integers, floats,
or booleans.


The parts in square brackets mean that you can pre-initialize your
variables if you wish:


VARIABLE ABoolean boolean true

VARIABLE AFloat float 5.6

VARIABLE AnInteger integer 4


It isn't necessary to pre-initialize variables, and if you choose not
to, then OpenZone will use zero as the default value for
integers and floats and false for
booleans. If you do pre-initialize variables, however,
bear in mind that the initial value cannot be an expression: it must be
a simple value (an actual number or boolean value).


Main code


The main code is the meat of your script: what it does.
There are a number of available statements you can use to accomplish
what you set out to do:


Statements to create triangles and rectangles


TRIANGLE  x1,y1,z1, x2,y2,z2, x3,y3,z3, texture,
\transparent\, \semitransparent\, \solid\, \color\,
\hascolor\, \masked\, \hasnormal\, \nx1, ny1, nz1, nx2, ny2, nz2,
nx3, ny3, nz3\


TRIANGLETEX  x1,y1,z1, x2,y2,z2, x3,y3,z3, tx1,tz1, tx2,tz2,
tx3,tz3, texture, \transparent\, \semitransparent\, \solid\,
\color\, \hascolor\, \masked\, \hasnormal\, \nx1, ny1, nz1,
nx2, ny2, nz2, nx3, ny3, nz3\


RECTANGLE  x1,y1,z1, x2,y2,z2, x3,y3,z3, x4,y4,z4, texture,
\transparent\, \semitransparent\, \solid\, \color\,
\hascolor\, \masked\, \hasnormal\, \nx1, ny1, nz1, nx2, ny2, nz2,
nx3, ny3, nz3, nx4, ny4, nz4\


RECTANGLETEX  x1,y1,z1, x2,y2,z2, x3,y3,z3, x4,y4,z4, tx1,tz1,
tx2,tz2, tx3,tz3, tx4,tz4, texture, \transparent\,
\semitransparent\, \solid\, \color\, \hascolor\, \masked\,
\hasnormal\, \nx1, ny1, nz1, nx2, ny2, nz2, nx3, ny3, nz3, nx4, ny4,
nz4\


Control-transfer statements


IF expression

\...

\ELSEIF expression\

\...

\ELSE\

\...

ENDIF



WHILE expression

\...

WEND



REPEAT

\...

UNTIL expression


Statements to execute other scripts in the script library


SCRIPT script-name xloc,yloc,zloc, xrot,yrot,zrot,
xsize,ysize,zsize, script-parameters


VARSCRIPT  script-name xloc,yloc,zloc, xrot,yrot,zrot,
xsize,ysize,zsize, script-parameters


Database statements


SQLPARM  table-name, column-name, value


If you're familiar with the BASIC language, then WindScript should
look very familiar. It doesn't have subroutines or line numbers, but
otherwise I borrowed heavily from BASIC to make WindScript as easy to
learn and use as possible. The items you see above in square brackets
(\) are optional. For instance, you can have an
if\...endif block without any else or
elseif clause if you don't need them. As a second
example, you can omit the color argument to a
rectangletex statement if you don't need one but you will
need to have a solid parameter if you also want to use a
color argument.


Notes on statements for creating triangles and rectangles


The only difference between triangle and 
triangletex statements is that triangletex
statements also let you set the texture coordinates at the vertices. The
same goes for rectangle and  rectangletex
statements.


The transparent, semitransparent, solid, hascolor, masked, and
hasnormal parameters are  boolean parameters. The color
parameter is an integer, and the nxx parameters after
hasnormal are floats. Some notes on these
parameters:


- Transparent overrides semitransparent.

- When solid evaluates to false it means that the player can
pass through the polygon.

- The color parameter may be entered as a
hexadecimalScripting_language_expressions number.

- When hascolor evaluates to false it means that the color value
is to be ignored.

- When masked evaluates to true it means that the polygon's
texture should be interpreted as a masked texture, like tree
leaves.

- When hasnormal evaluates to true the nxx parameters are taken
as the polygon's vertex normals. Otherwise, they are ignored if present
and the vertex normals are set equal to the polygon's normal.


Notes on control-transfer statements


if statements require endif statements terminating
them. You can have as many elseif clauses in between them
as you want, but only one else clause.


ExpressionsScripting_language_expressions in if,
while, and until statements do
not have to be in parentheses (like they do in C).
Remember, WindScript is modeled after BASIC.


Just like for if and endif statements,
while statements require terminating wend
statements.


In general, you can nest control-transfer statements as much as you
want.


Notes on script-execution statements


script calls are the sole exception to the rule that statements
be on only one line. You can place the script parameters on as many
lines as you wish: the parser that grabs the script parameters is
different from the main parser. Here is an example script call:


Script Box -0.5 + OverhangX, -0.5 + OverhangY, 0,

 0,0,0,

 LegThick, LegThick, SeatHeight - SeatThick / 2,

 Tex1;


The very first thing to note is the semicolon (;) at the end of the
script statement. This is how OpenZone knows where the
statement ends, and is absolutely necessary for all script
statements, even if they are on only one line. The first
nine parameters after the script name correspond to the
Loc, Heading, and Size
parameters that all objects in an .SCN file have. You're basically
telling OpenZone where the scripted object is, where it points, and how
big it is. After those, then supply a comma-delimited list of the
parameters the script itself takes. In the example above, we're only
supplying the first parameter (tex1) and are letting the script choose
defaults for all the others.


varscript works just like script but the script
name is contained in a variable or parameter. This is highly
experimental (and probably won't work), but here's the idea: what if
you've created a standard list of scripts, like a bunch of different
roof scripts that all take the same parameters? What if you just want
the user to be able to choose among them? An example might be a house
script, where two parameters were doorway type (square or arched) and
roof type (flat or peaked). It's a way of having geometry customization
be more modular (and reusable) instead of only in one script. I'm not
sure if it will work, but I think it's a useful feature.


Notes on database statements


sqlparm is a special command that is used when placing doors
into your zoneThe_mesh_library. It operates on all objects at the same
group level or below and allows you to set values for a specific table
and column (at this time, the table must always be "doors"). It lets
you use OpenZone to set any additional database parameters for objects
such as doors that are managed by a database.


Comments


WindScript only supports single-line comments, not block comments.
Comments always begin with a single open quote (') and can be placed
anywhere, even after a WindScript statement (though I don't know if
putting them in the middle of a multi-line script call will work \-- try
it and see).


Adding new scripts


To add a new script, you need to create an .SCP script file and a
corresponding .BMP bitmap for the script palette. Put both files in the
library\scripts folder, and OpenZone will automatically
load and attempt to compile the script when it starts up. If there's a
problem with the script you can look at the script log by clicking
 View\...View script log from the main menu.
 {style="margin-top:3pt;margin-bottom:3pt;"}
113(#_NDEF_113 "{$} Scripting language expressions"){#_NREF_113}114(#_NDEF_114 "{#} Scripting_language_expressions"){#_NREF_114}115(#_NDEF_115 "{K} Scripting language expressions"){#_NREF_115}116(#_NDEF_116 "{+} Scripted objects_WindScript:000"){#_NREF_116}}Scripting
language expressions 


Scripting language expressions


This section describes expressions you can put into if,
elseif, while, and until
statements.


While integer, float, and
boolean parameter and variable values have to evaluate to
those types, that doesn't mean they have to be numbers or
booleans. OpenZone includes an expression evaluator that can understand
formulas and calculate the answer. Put in "sin(3.2)", for instance,
and OpenZone will figure out the answer. Enter "3/5.5", and OpenZone
will perform the division. Enter "floor(44.5/9)"? No problem. Here are
the tokens and functions it understands:


- Numeric operators: +, -, \*, /

- String concatenation: +

- Modulo division: %, mod

- Logical or numeric and: &, &&, and

- Logical or numeric or: \|, \|\|, or

- Logical or numeric xor: \^, \^\^, xor

- Logical or numeric not: !, \~, not

- Equality: =, ==

- Inequality: !=, \<\>

- Less than, greater than, etc.: \<, \>, \<=, \>=

- Numeric functions: trunc(), round(), roundup(), frac(),
sgn(), abs(), int(), floor(), ceil(), sqrt(), sin(), cos(), tan(),
exp(), ln(), log10(), log2(), sqr()

- Integer shift left: \<\<, shl

- Integer shift right: \>\>, shr

- Boolean constants: true, false

- Angle determined by two numbers: @


Notice the "logical or numeric" operators: and, or, xor, and not.
OpenZone is aware of integer, float,
boolean, string, and  script
types, and when using these operators OpenZone will do whatever is
appropriate. There is no need to do what we do in C or C++, where "&&"
works only on booleans and "&" only on
integers: OpenZone is smarter than that. Where you see
more than once choice for the same thing (& or &&, for example), use
whatever feels comfortable. They mean the same thing to OpenZone. The
only exception is that the single equal sign (=) must be
used when assigning values to variables (see below).


Given this, you could code a parameter statement like this:


PARAM NumSteps Integer Default 10\*Sin(3)


And it would work. Actually, the expression evaluator is critical for
variable assignments (variables aren't much use if you can't change
their value):


A = A \* 3.5 \* Sin(B/2) \* Trunc(C/D)


You can enter numbers in hexadecimal if you wish, as long as you follow
Pascal syntax (once again, case doesn't matter):


\$0123ABCD (instead of 0x0123ABCD)


You can even enter expressions in the properties pane in
the main screen. You can enter parameter values as expressions if you
wish.


When writing scripts, the evaluator also recognizes several internal
variables:


- placex, placey, and placez,
the X, Y, and Z locations of the object, respectively,


- rotatex, rotatey, and
rotatez, the angles in which the object points,


- scalex, scaley, and scalez,
the object's size, and


- pi, the value 3.14159\...


Notes on trigonometric functions


The trigonometric functions (sin, cos, etc.) expect the angles you
supply to be in degrees (not radians). The reason for this
is because it's easiest for users to enter angles this way in the
 properties pane.


The at sign operator (@) calculates the angle determined by two
numbers, where the number preceding the operator is the x value and the
number after it is the y value. For example, 1\@0 would return 0
degrees, as would 5\@0 (or anything where the second number is zero).
0\@1 would return 90 degrees. The returned angle is always positive. If
you consider an angle a, then the first number is basically r\*cos(a)
and the second number is r\*sin(a).
 {style="margin-top:3pt;margin-bottom:3pt;"}
117(#_NDEF_117 "{$} A real-world example"){#_NREF_117}118(#_NDEF_118 "{#} A_real-world_example"){#_NREF_118}119(#_NDEF_119 "{K} A real-world example"){#_NREF_119}120(#_NDEF_120 "{+} Scripted objects_WindScript:000"){#_NREF_120}}A
real-world example 


A real-world example


Here we'll walk through a real script that comes with OpenZone, one
bit at a time to see how it all works. If you haven't already done so,
you should read the section describing
WindScriptWindScript:\_the_scripting_language. To get the most out of
this, create an object with this script and assign it the
"chain_links" texture. This texture comes with my sample textures
package: the tutorialTutorial contains instructions at the top as to
where you can get it.


This is the text of the "hanging_chain" script. Its purpose is to let
you take a chain texture and create an object of any length that uses
this texture. It's designed to repeat the texture as many times as it
has to to accommodate the length of the chain, such that you don't need
to worry about texture coordinates. All you have to do is specify the
texture, the width of the chain, and the overall size of the
object.


I've reformatted the comments to make them easier to read in a help
window, but otherwise this is the real script that comes with
OpenZone.


CATEGORY Furniture


PARAM ChainTex String

PARAM ChainWidth Float Default 0.2


Variable X1 Float

Variable X2 Float

Variable TZ Float

Variable Size Float


Size = SizeX

If Size \> SizeY

 Size = SizeY

EndIf


' Put in a sanity check to avoid divides by zero.

' OpenZone won't crash anyway, but this will prevent

' an error from being listed in the script parsing

' log if the user makes the object's size 0.


If (Size \<\> 0) And (ChainWidth \<\> 0)


 ' We always must scale to 1 when creating objects

 ' since the Size parameter multiplies everything

 ' we do. Therefore the width of the chain actually

 ' must \*decrease\* as the user enters larger values

 ' for the chain height.


 ' Divide the width by two because we want the object

 ' to be horizontally centered on the coordinate and

 ' therefore we'll build rectangles from -X1 to +X1.


 X1 = -(ChainWidth / 2) / Size


 ' Create a helper variable to make the RectangleTex

 ' statements easier to read.


 X2 = -X1


 ' The horizontal texture coordinates always should

 ' range from 0 to 1, but vertically it depends on

 ' how tall the chain is.


 TZ = SizeZ / (Size \* (X2 - X1))


 ' Draw the front face along the X axis


 RectangleTex X1,0,0, X1,0,1, X2,0,1, X2,0,0, 0,TZ, 0,0, 1,0, 1,TZ,
ChainTex, False, False, True, 0, False, True


 ' Draw the back face along the X axis


 RectangleTex X2,0,0, X2,0,1, X1,0,1, X1,0,0, 1,TZ, 1,0, 0,0, 0,TZ,
ChainTex, False, False, True, 0, False, True


 ' Draw the front face along the Y axis


 RectangleTex 0,X2,0, 0,X2,1, 0,X1,1, 0,X1,0, 0,TZ, 0,0, 1,0, 1,TZ,
ChainTex, False, False, True, 0, False, True


 ' Draw the back face along the Y axis


 RectangleTex 0,X1,0, 0,X1,1, 0,X2,1, 0,X2,0, 1,TZ, 1,0, 0,0, 0,TZ,
ChainTex, False, False, True, 0, False, True

EndIf


Looking at it step by step


The category statement


The first line in the script is contains the category
command, which tells OpenZone that this script should appear under a tab
called "furniture". You don't need to worry about capitalization of
the category name, since OpenZone contains logic to capitalize words as
necessary. Calling the category "furniture" or "FURNITURE" would
have the same effect.


The category line doesn't have to be the first line in the script, but
I like to put it there. If you don't include a category, OpenZone will
create a "default" category and put your script there.


Parameters available to users


This script defines two parameters that will show up in the
properties pane: ChainTex and ChainWidth. The ChainTex
parameter is where the user can specify the texture (e.g. chain_links)
and ChainWidth is the width of the chain in absolute units. The wider
the chain, the bigger the links will be.


Internal variables


Four variables are defined: X1, X2, TZ, and Size. X2 is just a helper
variable to make the script a little easier to understand.


You should probably read this next paragraph several times. When coding
scripted objects, always design the script as if the overall size
of the object is 1. So if you are making a script to create a
pyramid, for instance, code the script so that the base and height all
have size 1. The reason for this is because the Size
parameter on the properties pane will multiply anything
you do in the script to create an object at the size the user wants. Say
this to yourself several times: "every polygon I create in the script
is only for an object of size 1".


There is more to this though than just size: where you
center the object is just as important. When the user
creates a scripted object and specifies a location in the
Loc field, where should the center be? The exact center,
or only centered horizontally but at the bottom? At a corner? While you
are only interested in building an object of size 1 in the script, these
choices affect where you place it. You can choose to put polygons in the
range -0.5 to +0.5, for instance, or from 0 to 1 (or maybe 0 to 1 only
in the Z direction). Remember that the object will always be placed
relative to (0, 0, 0) in script space.


In the case of this chain, we are going to create polygons from -X1 to
X1 in the X and Y directions and from 0 to 1 in the Z direction. This
means that the Loc field will set the bottom location of
the chain, centered horizontally. You could conceivably change the
script so that it contains another parameter, SetTop, which when true
would make the polygons from -1 to 0 in the Z direction instead of from
0 to 1, thus letting the user set the location at the top of the chain
instead.


Main code


Now we are going to look at how this chain is created.


The first thing the code does is set the Size variable (this is
not the Size parameter in the
properties pane and is merely a local variable for this
script). SizeX and SizeY are global
variables and contain the values the user entered in the Size field in
the properties pane. The script is looking for the smaller
of the two values. Normally they will both be 1 since the Z size of the
chain is what determines the chain's length and the ChainWidth
parameter governs the chain's width.


Next is an if statement that makes sure the user entered
valid values, to avoid a divide-by-zero condition. OpenZone's script
runner is smart enough to look for conditions like this and won't crash
even if they happen, but it would put an error line in the script
parsing log in that event. Since this would only happen if the user did
something dumb, let's avoid putting something into the log if we can
avoid it. The parsing log's main purpose is to help you debug errors in
scripts as you write them, not make sure users put in valid values.
That's the script's job, and potentially we could change the script to
use some kind of reasonable default if they enter an invalid
value.


I indented the statements in the if\...endif block, but
it isn't necessary. Still, it's good practice to indent blocks to make
them easier to understand.


Two things are of note in the calculation of X1: first, we are
centering on 0 in the X and Y directions, so we are dividing the chain
width by 2 to create rectangles on either side of 0. Second, since we
only want to create an object of size 1, we are dividing by the Size
variable. OpenZone will automatically multiply any polygons by the
Size parameter from the properties pane (not
to be confused with the Size variable) later on to create an object of
the proper size.


When we texture the chain object, we want to use the full width of the
texture across the width of the chain but the vertical texture
coordinate depends on the chain's height. If the chain is long, the
texture should repeat over and over, which translates to a vertical
texture coordinate that is greater than 1. If the chain is shorter than
it is wide, then the coordiate will be less than 1. In our calculation
we are taking the ChainWidth value into account as well as the vertical
size of the object that the user wants.


Once we have the values we want, it's time to create the chain. It
consists of four rectangles: two in the X-Z plane (one for the front and
one for the back) and two in the Y-Z plane. Polygons created by the
triangle, triangletex,
rectangle, and  rectangletex statements are
always one-sided: you can only see them from one direction. For
something like this chain, then, we need to create polygons on both
sides so it can be seen from any direction. The extra pair of rectangles
in the Y-Z plane are so the chain doesn't look paper thin when looking
at it edge-on. This way there will always be polygons facing the
observer.


In the rectangletex statements we're also saying that
the polygon is solid (the player can't pass through it) and it uses a
masked texture. This makes sure that we can only see the actual chain
links in the texture and can see through the rest. The chain_links
texture file contains alpha information to tell OpenZone which parts are
transparent and which aren't.


------------------------------------------------------------------------

1(#_NREF_1){#_NDEF_1} OpenZone

2(#_NREF_2){#_NDEF_2} OpenZone

3(#_NREF_3){#_NDEF_3} OpenZone

4(#_NREF_4){#_NDEF_4} About:000

5(#_NREF_5){#_NDEF_5} OpenZone tutorials

6(#_NREF_6){#_NDEF_6} OpenZone_tutorials

7(#_NREF_7){#_NDEF_7} OpenZone tutorials

8(#_NREF_8){#_NDEF_8} Tutorials:000

9(#_NREF_9){#_NDEF_9} Tutorial

10(#_NREF_10){#_NDEF_10} Tutorial

11(#_NREF_11){#_NDEF_11} Tutorial

12(#_NREF_12){#_NDEF_12} Tutorials:000

13(#_NREF_13){#_NDEF_13} Mob Modeling 1: Creating a
new creature race

14(#_NREF_14){#_NDEF_14}
Mob_Modeling_1:\_Creating_a\_new_creature_race

15(#_NREF_15){#_NDEF_15} Mob Modeling 1: Creating a
new creature race

16(#_NREF_16){#_NDEF_16} Tutorials:000

17(#_NREF_17){#_NDEF_17} Mob Modeling 2: Altering your
new creature

18(#_NREF_18){#_NDEF_18}
Mob_Modeling_2:\_Altering_your_new_creature

19(#_NREF_19){#_NDEF_19} Mob Modeling 2: Altering your
new creature

20(#_NREF_20){#_NDEF_20} Tutorials:000

21(#_NREF_21){#_NDEF_21} Mob Modeling 3: Altering your
creature's structure

22(#_NREF_22){#_NDEF_22}
Mob_Modeling_3:\_Altering_your_creature's_structure

23(#_NREF_23){#_NDEF_23} Mob Modeling 3: Altering your
creature's structure

24(#_NREF_24){#_NDEF_24} Tutorials:000

25(#_NREF_25){#_NDEF_25} Mob Modeling 4: Altering your
animations

26(#_NREF_26){#_NDEF_26}
Mob_Modeling_4:\_Altering_your_animations

27(#_NREF_27){#_NDEF_27} Mob Modeling 4: Altering your
animations

28(#_NREF_28){#_NDEF_28} Tutorials:000

29(#_NREF_29){#_NDEF_29} Mob Modeling 5: Adding
variants

30(#_NREF_30){#_NDEF_30}
Mob_Modeling_5:\_Adding_variants

31(#_NREF_31){#_NDEF_31} Mob Modeling 5: Using your
new creature with OpenZone

32(#_NREF_32){#_NDEF_32} Tutorials:000

33(#_NREF_33){#_NDEF_33} Mob Modeling 6: Using your
new creature with OpenZone

34(#_NREF_34){#_NDEF_34}
Mob_Modeling_6:\_Using_your_new_creature_with_OpenZone

35(#_NREF_35){#_NDEF_35} Mob Modeling 6:

36(#_NREF_36){#_NDEF_36} Tutorials:000

37(#_NREF_37){#_NDEF_37} The main window

38(#_NREF_38){#_NDEF_38} The_main_window

39(#_NREF_39){#_NDEF_39} The main window

40(#_NREF_40){#_NDEF_40} Introduction:000

41(#_NREF_41){#_NDEF_41} OpenZone folders

42(#_NREF_42){#_NDEF_42} OpenZone_folders

43(#_NREF_43){#_NDEF_43} OpenZone folders

44(#_NREF_44){#_NDEF_44} Introduction:000

45(#_NREF_45){#_NDEF_45} Creating a new zone from
scratch

46(#_NREF_46){#_NDEF_46}
Creating_a\_new_zone_from_scratch

47(#_NREF_47){#_NDEF_47} Creating a new zone from
scratch

48(#_NREF_48){#_NDEF_48} Using OpenZone:000

49(#_NREF_49){#_NDEF_49} The Ground Editor

50(#_NREF_50){#_NDEF_50} The_Ground_Editor

51(#_NREF_51){#_NDEF_51} The Ground Editor

52(#_NREF_52){#_NDEF_52} Using OpenZone:000

53(#_NREF_53){#_NDEF_53} Adding water, lava or PvP
areas

54(#_NREF_54){#_NDEF_54}
Adding_water,\_lava_or_PvP_areas

55(#_NREF_55){#_NDEF_55} Adding water, lava or PvP
areas

56(#_NREF_56){#_NDEF_56} Using OpenZone:000

57(#_NREF_57){#_NDEF_57} The mesh library

58(#_NREF_58){#_NDEF_58} The_mesh_library

59(#_NREF_59){#_NDEF_59} The mesh library

60(#_NREF_60){#_NDEF_60} Using OpenZone:000

61(#_NREF_61){#_NDEF_61} Looking at your zone and
moving around in it

62(#_NREF_62){#_NDEF_62}
Looking_at_your_zone_and_moving_around_in_it

63(#_NREF_63){#_NDEF_63} Looking at your zone and
moving around in it

64(#_NREF_64){#_NDEF_64} Using OpenZone:000

65(#_NREF_65){#_NDEF_65} Altering your ground's
elevation

66(#_NREF_66){#_NDEF_66}
Altering_your_ground's_elevation

67(#_NREF_67){#_NDEF_67} Altering your ground's
elevation

68(#_NREF_68){#_NDEF_68} Using OpenZone:000

69(#_NREF_69){#_NDEF_69} Editing overall zone
properties

70(#_NREF_70){#_NDEF_70}
Editing_overall_zone_properties

71(#_NREF_71){#_NDEF_71} Editing overall zone
properties

72(#_NREF_72){#_NDEF_72} Using OpenZone:000

73(#_NREF_73){#_NDEF_73} Manipulating objects

74(#_NREF_74){#_NDEF_74} Manipulating_objects

75(#_NREF_75){#_NDEF_75} Manipulating objects

76(#_NREF_76){#_NDEF_76} Using OpenZone:000

77(#_NREF_77){#_NDEF_77} Adding a light source to your
zone

78(#_NREF_78){#_NDEF_78}
Adding_a\_light_source_to_your_zone

79(#_NREF_79){#_NDEF_79} Adding a light source to your
zone

80(#_NREF_80){#_NDEF_80} Using OpenZone:000

81(#_NREF_81){#_NDEF_81} General OpenZone display
options

82(#_NREF_82){#_NDEF_82}
General_OpenZone_display_options

83(#_NREF_83){#_NDEF_83} General OpenZone display
options

84(#_NREF_84){#_NDEF_84} Using OpenZone:000

85(#_NREF_85){#_NDEF_85} Exporting your zone

86(#_NREF_86){#_NDEF_86} Exporting_your_zone

87(#_NREF_87){#_NDEF_87} Exporting your zone

88(#_NREF_88){#_NDEF_88} Using OpenZone:000

89(#_NREF_89){#_NDEF_89} Importing objects and entire
zones

90(#_NREF_90){#_NDEF_90}
Importing_objects_and_entire_zones

91(#_NREF_91){#_NDEF_91} Importing objects and entire
zones

92(#_NREF_92){#_NDEF_92} Using OpenZone:000

93(#_NREF_93){#_NDEF_93} Adding an OpenZone scene to
your existing zone

94(#_NREF_94){#_NDEF_94}
Adding_an_OpenZone_scene_to_your_existing_zone

95(#_NREF_95){#_NDEF_95} Adding an OpenZone scene to
your existing zone

96(#_NREF_96){#_NDEF_96} Using OpenZone:000

97(#_NREF_97){#_NDEF_97} Creating and exporting
creatures with Anim8or

98(#_NREF_98){#_NDEF_98}
Creating_and_exporting_creatures_with_Anim8or

99(#_NREF_99){#_NDEF_99} Creating and exporting
creatures with Anim8or

100(#_NREF_100){#_NDEF_100} Using OpenZone:000

101(#_NREF_101){#_NDEF_101} Adding object placement
hotspots

102(#_NREF_102){#_NDEF_102}
Adding_object_placement_hotspots

103(#_NREF_103){#_NDEF_103} Adding object placement
hotspots

104(#_NREF_104){#_NDEF_104} Using OpenZone:000

105(#_NREF_105){#_NDEF_105} The scripted objects
panel

106(#_NREF_106){#_NDEF_106}
The_scripted_objects_panel

107(#_NREF_107){#_NDEF_107} The scripted objects
panel

108(#_NREF_108){#_NDEF_108} Scripted objects:000

109(#_NREF_109){#_NDEF_109} WindScript: the scripting
language

110(#_NREF_110){#_NDEF_110}
WindScript:\_the_scripting_language

111(#_NREF_111){#_NDEF_111} WindScript: the scripting
language

112(#_NREF_112){#_NDEF_112} Scripted
objects_WindScript:000

113(#_NREF_113){#_NDEF_113} Scripting language
expressions

114(#_NREF_114){#_NDEF_114}
Scripting_language_expressions

115(#_NREF_115){#_NDEF_115} Scripting language
expressions

116(#_NREF_116){#_NDEF_116} Scripted
objects_WindScript:000

117(#_NREF_117){#_NDEF_117} A real-world example

118(#_NREF_118){#_NDEF_118} A_real-world_example

119(#_NREF_119){#_NDEF_119} A real-world example

120(#_NREF_120){#_NDEF_120} Scripted
objects_WindScript:000
