LIB = "packages/"
#LIB = NULL

# These were not on CRAN, so it did not install to "packages/"
# devtools::install_github ('ropenscilabs/miner')
library (miner)
library (craft)

library (igraph)
library (Rmaze)

# Material constants
AIR = 0

STONE = 1
DIRT = 3

WATER_STATIONARY = 9
LAVA_STATIONARY = 11

WOOL = 35
GLASS = 20
BRICK = 45
DIAMOND_BLOCK = 57

WOOD_PLANKS = 125

STAINED_GLASS_BLOCK = 95
STAINED_GLASS_PANE = 160

TORCH = 50
DOOR_WOOD = 64
STONE_PRESSURE_PLATE = 70
FENCE = 85
CARPET = 171


# Color constants
WHITE = 0
ORANGE = 1
MAGENTA = 2
LIGHT_BLUE = 3
YELLOW = 4
LIME = 5
PINK = 6
GREY = 7
LIGHT_GREY = 8
CYAN = 9
PURPLE = 10
BLUE = 11
BROWN = 12
GREEN = 13
RED = 14
MAX_COLOR = BLACK = 15

# Direction is "E (+x), W (-x), S (+z), or N (-z), 
# 0 for E, 1 for W, 2 for S, 3 for N.

# These match the stair style ID.
EAST <- 0
WEST <- 1
SOUTH <- 2
NORTH <- 3

# Door style constants. Or or add these together for the style ID.
DOOR_TOP = 0x8
DOOR_BOTTOM = 0x0

DOOR_OPEN = 0x4
DOOR_CLOSED = 0x0

# Note that these do NOT match the stair style ID.
DOOR_EAST = 0x0
DOOR_SOUTH = 0x1
DOOR_WEST = 0x2
DOOR_NORTH = 0x3


# General functions for connecting to the game world:

# mc_connect: Connect to a Minecraft server
# chatPost: Make a chat message appear to players in their clients
# getChatPosts: Retrieve the most recent chat message
# mc_close: Close the connection to the Minecraft server


# This is the local host IP. Connects to the Minecraft server on this machine.
mcConnect <- function (ip = '127.0.0.1') {
   mc_connect (ip)
   
   chatPost ("R connection established!")
}


mcClose <- function () {
   mc_close ()
}


# Functions for interacting with player location and direction (the angle of
# their gaze). Note that in Minecraft, x is East, y is Up and z is South, and
# the unit is one block. (A block is a 1-unit cube.) The player figure is about
# 2 units tall.

# getPlayerIds: Get a list of players in the world
# getPlayerPos: Find the position of a specified player
# getPlayerDirection: Get a unit vector representing player direction
# getPlayerRotation: Return the angle of the player's direction in the x-z plane
# getPlayerPitch: Return the up/down angle of the player's direction
# setPlayerPos: Teleport a player to a specified location


# Functions for interacting with the Minecraft world.

# getHeight: Find the height of the world (the y value of the highest non-air point) at a given x-z location
# getBlock: Return the type of block (air, wood etc) in a given location, as a code. See the provided data frame mc_items for a list of blocks and item codes.
# setBlock: change the block type at a given location. Use this to place items in the world.
# getBlocks: Return the block types in a rectangular region, as a 3-dimensional array.
# setBlocks: Fill a rectangular region with a specific block type
# getBlockHits: Return the locations of blocks recently struck by a player's iron sword

whereami <- function (playerId = NULL)
{
   getPlayerPos (playerId, tile = T)
}

# Allows you to set a cuboid of blocks, but also specify the style ID (e.g., color,
# direction of stairs)
setBlocksStyle <- function (x0, y0, z0, x1, y1, z1, id, style = 0, debugFlag = FALSE)
{
   for (x in x0:x1) {
      for (y in y0:y1) {
         for (z in z0:z1) {
            if (debugFlag) { print (paste ("set block at:", x, y, z, collapse = " ")) }
            setBlock (x, y, z, id, style)
         }
      }
   }
}


# Allows you to set a cuboid of blocks with a random percentages of different 
# blocks or block styles. Does not support different style vectors per block.
# 
# Example: stained glass color mix
# setBlocksMix (77, 28, -1033, 33, 38, -1033, 160, 
#               c (.25, .2, .2, .2, .15), 
#               style = c (BLUE, LIGHT_BLUE, PURPLE, MAGENTA, GREEN))

setBlocksMix <- function (x0, y0, z0, x1, y1, z1, ids, probs, styles = 0, 
                          debugFlag = FALSE)
{
   # Length times width times height   
   totalBlocks <- (abs (x1 - x0) + 1) * (abs (y1 - y0) + 1) * (abs (z1 - z0) + 1)
   if (debugFlag) print (paste ("totalBlocks:", totalBlocks))
   
   if (length (ids) > 1) {
      if (length (ids) != length (probs)) {
         print ("Number of block IDs does not match number of probabilities.")
         return ()
      }
      # Create the total array of block IDs to set.
      blockArray <- sample (ids, totalBlocks, replace = TRUE, probs)
   } else {

      # Repeat the same block throughout the cuboid.
      blockArray <- rep (ids, totalBlocks)
   }
   if (debugFlag) print (paste (c("blockArray:", blockArray), collapse = " "))

   if (length (styles) > 1) {
      if ((length (ids) > 1) && (length (ids) != length (styles))) {
         print ("Number of styles does not match number of block IDs.")
         return ()
      }
      
      # Create the total array of styles to set.
      styleArray <- sample (styles, totalBlocks, replace = TRUE, probs)
   } else {
      
      # Repeat the same style throughout the cuboid.
      styleArray <- rep (styles, totalBlocks)
   }
   if (debugFlag) print (paste (c("styleArray:", styleArray), collapse = " "))
   
   # Build the specified blocks and styles.
   i <- 1
   for (x in x0:x1) {
      for (y in y0:y1) {
         for (z in z0:z1) {
            if (debugFlag) print (paste ("i:", i, 
                                         "setting x:", x, "y: ", y, "z:", z,
                                         "block:", blockArray[i], 
                                         "style:", styleArray[i], collapse = " "))
            
            setBlock (x, y, z, blockArray[i], styleArray[i])
            i <- i + 1
         }
      }
   }
}


# Clears out a cube around the player.
clearSpace <- function (length, playerId = NULL) {
   pos <- getPlayerPos (playerId, tile = TRUE)
   pos[1] <- pos[1] - length / 2
   pos[3] <- pos[3] - length / 2
   
   setBlocks (pos[1], pos[2], pos[3], 
              pos[1] + length, pos[2] + length, pos[3] + length, AIR)
}


# Returns the opposite direction
flipDir <- function (direction) {
   direction <- direction + 1
   if (direction == NORTH + 1) {
      direction <- SOUTH
   }
   else if (direction == WEST + 1)
      direction <- EAST
   
   return (direction)
}


# Figure out the largest value of direction and translate to a compass direction.
getPlayerCompassDir <- function (playerId = NULL) {
   vec <- getPlayerDirection (playerId)

   # E (+x), W (-x), S (+z), or N (-z)
   # See if magnitude of x is bigger than magnitude of z. Ignore y.
   if (abs(vec[1]) > abs(vec[3])) {
      if (vec[1] > 0)
         return (EAST)
      else
         return (WEST)
   } else {
      if (vec[3] > 0)
         return (SOUTH)
      else
         return (NORTH)
   }
}


#' Draws a line
#' 
#' Implements Bresenham's line-drawing algorithm in 3D space. Takes two points,
#' where each point is a vector of 3 coordinates, and constructs a line between
#' the two points.
#' Based on the Python code here:
#' https://gist.github.com/theJollySin/2ba8fbbb7d008b0d1f15
#' @param p0 vector of the first endpoint
#' @param p1 vector of the second endpoint
#' @param blockType Minecraft block ID to draw the line with
#' @param blockStyle Minecraft block style ID to draw the line with (e.g., color)
#' @param debugFlag Set to true to get debugging information printed to console
#' 
#' @export

drawLine <- function (p0, p1, blockType = STONE, blockStyle = 0, 
                      debugFlag = TRUE) {
   x <- x0 <- p0[1]
   y <- y0 <- p0[2]
   z <- z0 <- p0[3]
   
   x1 <- p1[1]
   y1 <- p1[2]
   z1 <- p1[3]
   
   # Calculate deltas.
   dx = x1 - x0
   dy = y1 - y0
   dz = z1 - z0
   
   # Calculate the increments.
   sx <- sign (dx)
   sy <- sign (dy)
   sz <- sign (dz)
   
   # Figure out just the magnitude of change so we can decide which
   # variable to loop/count through
   dx = abs (dx)
   dy = abs (dy)
   dz = abs (dz)
   
   if (debugFlag) print (paste ("dx: ", dx, ", dy: ", dy, 
                                ", dz: ", dz, 
                                "sx: ", sx, ", sy: ", sy, 
                                ", sz: ", sz))
   
   # The largest difference is the one we'll loop/count through, cuz the other 
   # variables may or may not increase by 1 for each increase of the one with 
   # the largest difference. If the max is a tie, it doesn't matter which one it
   # loops through.
   
   # dz is the largest    
   if ((dz > dx) && (dz > dy))
   {
      if (debugFlag) print ("dz largest")
      
      err_x <- dz / 2.0
      err_y <- dz / 2.0
      
      while (z != z1) {
         if (debugFlag) print (paste ("adding: ", x, y, z))
         setBlock (x, y, z, blockType, blockStyle)
         
         err_x <- err_x - dx
         if (err_x < 0) {
            x <- x + sx
            err_x <- err_x + dz
         }
         err_y <- err_y - dy
         if (err_y < 0) {
            y <- y + sy
            err_y <- err_y + dz
         }
         z <- z + sz
      }
      
      # dx largest
   } else if (dx > dy) {
      if (debugFlag) print ("dx largest")
      
      err_z = dx / 2.0
      err_y = dx / 2.0
      while (x != x1) {
         if (debugFlag) print (paste ("adding: ", x, y, z))
         setBlock (x, y, z, blockType, blockStyle)
         
         err_y <- err_y - dy
         if (err_y < 0) {
            y <- y + sy
            err_y <- err_y + dx
         }
         err_z <- err_z - dz
         if (err_z < 0) {
            z <- z + sz
            err_z <- err_z + dx
         }
         x <- x + sx
         
      } 
      
   # dy largest
   } else {
      if (debugFlag) print ("dy largest")
      
      err_x = dy / 2.0
      err_z = dy / 2.0
      while (y != y1) {
         if (debugFlag) print (paste ("adding: ", x, y, z))
         setBlock (x, y, z, blockType, blockStyle)
         
         err_x <- err_x - dx
         if (err_x < 0) {
            x <- x + sx
            err_x <- err_x + dy
         }
         err_z <- err_z - dz
         if (err_z < 0) {
            z <- z + sz
            err_z <- err_z +  dy
         }
         y <- y + sy
      }
   }
   
   if (debugFlag) print (paste ("adding: ", p1[1], p1[2], p1[3]))
   setBlock (p1[1], p1[2], p1[3], blockType, blockStyle)
}


#' Builds stairs
#' 
#' Build stairs up or down from player position in the specified direction.
#' For now, width should be odd so that we can have a central staircase and 
#' extend to the right and left of it to widen it.
#' @param down Whether to build stairs downward. If false, builds upward
#' @param stopHt The height at which to stop building.
#' @param stopOnNotAir Whether to stop if about to build where something exists.
#' @param width How wide to build the staircase. MUST BE AN ODD NUMBER
#' @param stairId The block ID for the stairs.
#' @param blockId The block ID for the block underneath each stair. Can set to AIR
#'    if you don't want this block.
#' @param debugFlag Set true if you want some debugging information printed to the
#'    console
#' @export

buildStairs <- function (down = TRUE, stopHt = 0, stopOnNotAir = TRUE,
                         width = 1, stairId = 109, blockId = 98, 
                         buildBlock = TRUE, debugFlag = FALSE)
{
   # Get current player position and heading.
   Pos <- getPlayerPos (tile = TRUE)
   x <- Pos[1]
   y <- Pos[2]
   z <- Pos[3]
   
   if (down) y <- y - 1
   
   direction <- getPlayerCompassDir ()

   if (debugFlag) print (paste ("x: ", x, ", y: ", y, ", z: ", z, ", 
                                dir: ", direction))

   # Make sure the input makes sense.
   checkDown <- (stopHt > y)
   if (!(xor (checkDown, down))) {
      print (paste ("Input invalid. Current height: ", y, " Stop height: ",
                    stopHt, "Down? ", down))
      return ()
   }

   # Calculate how many blocks to extend in either direction from the middle.
   # (split this into L and R components that differ if width is even)
   widthExtend <- ceiling ((width - 1) / 2)
   
   # Set zInc and xInc based on direction. 
   # E (+x), W (-x), S (+z), or N (-z)
   zInc <- 0
   xInc <- 0
   if (direction == NORTH) {
      zInc <- -1
      zWidthInc <- 0
      xWidthInc <- 1

   } else if (direction == SOUTH) {
      zInc <- 1
      zWidthInc <- 0
      xWidthInc <- 1

   } else if (direction == EAST) {
      xInc <- 1
      xWidthInc <- 0
      zWidthInc <- 1   

   } else {
      xInc <- -1
      xWidthInc <- 0
      zWidthInc <- 1
   }

   # Set stairDir depending on direction and if going up or down.
   # Add validity check to make sure program will stop!
   if (down) {
      yInc <- -1
      stairDir <- flipDir (direction)
      stopHt <- stopHt - 1
   } else {
      yInc <- 1
      stairDir <- direction
      stopHt <- stopHt + 1
   }
   
   # Start one block in that direction (don't build right under the player)
   x <- x + xInc
   z <- z + zInc
   if (debugFlag) print (paste ("Adj x: ", x, ", y: ", y, ", z: ", z))

   # Build to the specified height.   
   while (y != stopHt) {

      # Check if the stair would destroy anything.
      if (stopOnNotAir) {
         AirCheck <- getBlocks ( x - (widthExtend * xWidthInc), y, 
                                 z - (widthExtend * zWidthInc),
                                 x + (widthExtend * xWidthInc), y, 
                                 z + (widthExtend * zWidthInc) )
         
         if (!(all (AirCheck == AIR))) {
            print ("Stopping before building stair into blocks!")
            if (debugFlag) print (paste ("Stop x: ", x, ", y: ", y, ", z: ", z))
            break
         }
      }
         
      # Build the stair.
      setBlocksStyle (x - (widthExtend * xWidthInc), 
                      y, z - (widthExtend * zWidthInc),
                      x + (widthExtend * xWidthInc), 
                      y, z + (widthExtend * zWidthInc),
                      stairId, stairDir)
      
      if (buildBlock)
      {
         # Check if the block under the stair would destroy anything.
         # ADD CHECK TO SKIP IF UP AND FIRST BLOCK!
         if (stopOnNotAir) {
            AirCheck <- getBlocks ( x - (widthExtend * xWidthInc), y - 1, 
                                    z - (widthExtend * zWidthInc),
                                    x + (widthExtend * xWidthInc), y - 1, 
                                    z + (widthExtend * zWidthInc) )
            
            if (!(all (AirCheck == AIR))) {
               print ("Stopping before building block into blocks!")
               if (debugFlag) print (paste ("Stop x: ", x, ", y: ", y, 
                                            ", z: ", z))
               break
            }
         }
         
         # Build the block beneath the stair.
         setBlocks (x - (widthExtend * xWidthInc), 
                    y - 1, z - (widthExtend * zWidthInc),
                    x + (widthExtend * xWidthInc), 
                    y - 1, z + (widthExtend * zWidthInc),
                    blockId)
      }

      # Advance to the next iteration.
      x <- x + xInc
      z <- z + zInc
      y <- y + yInc
   }
}

# Creates a building of specified dimensions and materials.
buildBuilding <- function (length = 8, width = 6, height = 35, color = BLUE, 
                           foundation = STONE, wall = BRICK, 
                           floorBoards = WOOD_PLANKS, carpet = CARPET, 
                           windowMaterial = GLASS, gap = AIR) 
{
   if (length == 0) {
      length = sample (20:50, 1)
   }
   
   if (width == 0) {
      width = sample (10:40, 1)
   }

   if (height == 0) {      
      height = sample (10:50, 1)
   }
         
   # Get the player position
   pos <- getPlayerPos (tile = TRUE)
   x <- pos[1]
   y <- pos[2]
   z <- pos[3]

   x = x - (width / 2)
   z = z - (width / 2)
   
   # Build the foundation.
   setBlocks (x, y - 2, z, 
              x + width, y - 2, z + length, foundation)
   
   # Build the outer shell of the house
   setBlocks (x, y, z, 
              x + width, y + height, z + length, wall)
   
   # Carve the insides out with AIR
   setBlocks (x + 1, y, z + 1, 
              x + width - 1, y + height - 1, z + length - 1, gap)
   
   # Build the floor and carpet it. 
   setBlocks (x + 1, y - 1, z + 1, 
              x + width - 1, y - 1, z + length - 1, floorBoards)
   setBlocksStyle (x + 1, y, z + 1, 
                   x + width - 1, y, z + length - 1, carpet, color)
   
   # Build the door.
   setBlocks (x + width / 2, y, z, 
              x + width / 2, y + 1, z, gap)

   buildDoor (x + width / 2, y, z)   
   
   # Build a window. RANDOMIZE THESE!
   # setBlocks (x + 2, y + 2, z, 
   #            x + 3, y + 3, z, windowMaterial)
   # 
   # # Build another window
   # setBlocks (x, y + 3, z + 1, 
   #            x, y + 4, z + 2, windowMaterial)
   
}

buildRLogo <- function (playerId = NULL, height = 80, width = 70, debugFlag = TRUE)
{
   # For buildRLogo
   library (plyr, lib.loc = LIB)
   library (magrittr, lib.loc = LIB)
   library (imager, lib.loc = LIB)
   
   url <- "https://www.r-project.org/logo/Rlogo.png"
   file <- basename (url)
   download.file (url, file)

   # Next, let's use the imager package to read it into R.
   # library (imager, lib = LIB)
   logo <- load.image (file)

   # Print to see the size of the image.
   if (debugFlag) print (logo)

   # The image is stored as a 4-dimensional array: horizontal and vertical
   # position, time, and color.
   dim (logo)
   
   # Let's reduce the size to 80x70.
   logo <- resize (logo, height, width)

   # There is a bit of shading in the logo, but mostly there are three colors:
   # transparent, gray, and blue. If we look at a histogram of the first color
   # channel (red), we can see the three pieces:
   # par (mar = c (5.1, 4.1, 0.6, 0.6), las = 1)
   # hist (logo[,,,1], breaks=100, main="",
   #       xlab="Red color intensity")
   
   # So let's truncate at 0.05 and 0.4.
   logo[] <- cut (logo, c(-Inf, 0.05, 0.4, Inf))
   logo <- logo[,,1,1]

   # Render in Minecraft
   # Now let's load the miner package, connect to the MineCraft server, and find a
   # spot to place the logo.
   host_pos <- getPlayerPos (playerId, tile = TRUE)
   host_pos[2] <- host_pos[2] + 10
   print (host_pos)
   
   # Now let's render the R logo using gray and blue wool, leaving parts
   # transparent. First, we pick out the blue and gray wool blocks, to get the
   # item and style IDs.
   blue <- find_item ("Blue Wool")
   gray <- find_item ("Light Gray Wool")

   # Now we try rendering the logo.
   for (i in 1:nrow (logo)) {
      for (j in 1:ncol (logo)) {
         if (logo[i, j] == 2)
            setBlock (host_pos[1] + (nrow (logo) - i),
                      host_pos[2] + (ncol (logo) - j),
                      host_pos[3],
                      blue[2], blue[3])
         if(logo[i,j] == 3)
            setBlock (host_pos[1] + (nrow (logo) - i),
                      host_pos[2] + (ncol (logo) - j),
                      host_pos[3],
                      gray[2], gray[3])
      }
   }
}


#' Builds a door
#' 
#' Build a door in the wall (or any non-AIR block) that the player is looking at,
#' opening in that same direction. If desired, also puts a pressure plate in front
#' of and behind the door to automatically open it. Alternatively, can specify the 
#' coordinates and direction manually.
#' 
#' @param x, y, z coordinates of where to put the door. If NULL, will calculate
#'  from player's position and direction they are facing.
#' @param direction The compass direction the door should open in.
#' @param doorBlock The material to create the door out of.
#' @pressurePlate Whether to put pressure plates around the door.
#' @pressurePlateBlock The material to create the pressure plate out of.
#' @export

buildDoor <- function (x = NULL, y = NULL, z = NULL, direction = NULL, 
                       doorBlock = DOOR_WOOD, pressurePlate = TRUE, 
                       pressurePlateBlock = STONE_PRESSURE_PLATE, playerId = NULL) 
{
   # Find out where the player is looking.
   if (is.null (direction))
      direction <- getPlayerCompassDir ()

   # E (+x), W (-x), S (+z), or N (-z)
   xInc <- zInc <- 0
   if (direction == EAST) {
      doorDir <- DOOR_EAST
      xInc <- 1
   } else if (direction == WEST) {
      doorDir <- DOOR_WEST
      xInc <- -1
   } else if (direction == SOUTH) {
      doorDir <- DOOR_SOUTH
      zInc <- 1
   } else {
      doorDir <- DOOR_NORTH
      zInc <- -1
   }
   
   if (is.null (x) || is.null (y) || is.null (z)) {
      pos <- getPlayerPos (playerId, tile = TRUE)
      
      # Move in the direction the player is looking, and up one in case there
      # is carpet
      x <- pos[1] + xInc
      y <- pos[2] + 1
      z <- pos[3] + zInc
      isNotAir <- getBlock (x, y, z, FALSE)
      
      # Scan in the direction the player is looking until a non-air block is
      # found.
      while (isNotAir == AIR) {
         x <- x + xInc
         z <- z + zInc
         isNotAir <- getBlock (x, y, z, FALSE)
      }
      
      # Move back down to the floor.
      y <- y - 1
   }
   
   # Need to clear the space first, or else the top of the door will be covered.
   setBlocks (x, y, z, x, y + 1, z, AIR)

   # Create the door! It's two blocks high and the top and bottom blocks need
   # to be specifically specified.
   setBlock (x, y, z, doorBlock, DOOR_BOTTOM + doorDir)
   setBlock (x, y + 1, z, doorBlock, DOOR_TOP + doorDir)

   # If requested, add a pressure plate in front of and behind the door (thus
   # automatically opening it when you approach it!)
   if (pressurePlate) {
      setBlock (x + xInc, y, z + zInc, pressurePlateBlock)
      setBlock (x - xInc, y, z - zInc, pressurePlateBlock)
   }
}

# Fences a square area.
buildFence <- function (length = 8, fenceBlock = FENCE, 
                        digHole = TRUE, putCarpet = TRUE, playerId = NULL,
                        debugFlag = TRUE)
{
   totalLen <- 4 * length
   heightArray <- vector (length = totalLen)

   pos <- getPlayerPos (playerId, tile = TRUE)
   xStart <- pos[1] - length / 2
   yStart <- pos[2]
   zStart <- pos[3] - length / 2
   
   i <- 1
   x <- xStart
   
   if (debugFlag) print (paste ("i:", i, 
                                "xStart:", xStart, "yStart: ", yStart, 
                                "zStart:", zStart, collapse = " "))
   
   # Scope out the heights we'll be building.
   for (z in zStart:(zStart + length)) {
      if (debugFlag) print (paste ("1 i:", i, "x:", x, "z:", z, collapse = " "))
      heightArray[i] <- getHeight (x, z)
      setBlock (x, heightArray[i], z, fenceBlock)
      
      # Checks if there is a height disparity.
      if (z != zStart)
      {
         if (heightArray[i] < heightArray[i - 1])
         {
            setBlocks (x, heightArray[i], z, 
                       x, heightArray[i - 1], z, fenceBlock)
         } else if (heightArray[i] > heightArray[i - 1]) {
            setBlocks (x, heightArray[i - 1], z - 1, 
                       x, heightArray[i], z - 1, fenceBlock)
         } 
      }
      
      i <- i + 1
   }
   for (x in xStart:(xStart + length)) {
      if (debugFlag) print (paste ("2 i:", i, "x:", x, "z:", z, collapse = " "))
      heightArray[i] <- getHeight (x, z)
      setBlock (x, heightArray[i], z, fenceBlock)
      
      if (x != xStart)
      {
         if (heightArray[i] < heightArray[i - 1])
         {
            setBlocks (x, heightArray[i], z, 
                       x, heightArray[i - 1], z, fenceBlock)
         } else if (heightArray[i] > heightArray[i - 1]) {
            setBlocks (x - 1, heightArray[i - 1], z, 
                       x - 1, heightArray[i], z, fenceBlock)
         } 
      }
      
      i <- i + 1
   }
   for (z in (zStart + length):zStart) {
      if (debugFlag) print (paste ("3 i:", i, "x:", x, "z:", z, collapse = " "))
      heightArray[i] <- getHeight (x, z)
      setBlock (x, heightArray[i], z, fenceBlock)
      
      if (z != zStart + length)
      {
         if (heightArray[i] < heightArray[i - 1])
         {
            setBlocks (x, heightArray[i], z, 
                       x, heightArray[i - 1], z, fenceBlock)
         } else if (heightArray[i] > heightArray[i - 1]) {
            setBlocks (x, heightArray[i - 1], z + 1, 
                       x, heightArray[i], z + 1, fenceBlock)
         } 
      }
      
      i <- i + 1
   }
   for (x in (xStart + length):xStart) {
      if (debugFlag) print (paste ("4 i:", i, "x:", x, "z:", z, collapse = " "))
      heightArray[i] <- getHeight (x, z)
      setBlock (x, heightArray[i], z, fenceBlock)
      
      if (x != xStart + length)
      {
         if (heightArray[i] < heightArray[i - 1])
         {
            setBlocks (x, heightArray[i], z, 
                       x, heightArray[i - 1], z, fenceBlock)
         } else if (heightArray[i] > heightArray[i - 1]) {
            setBlocks (x + 1, heightArray[i - 1], z, 
                       x + 1, heightArray[i], z, fenceBlock)
         } 
      }
      
      i <- i + 1
   }
   
#   return (heightArray)
}

# r1 is biggest radius, r3 is smallest

drawNestedDonuts <- function (r1, r2, r3, blockId1, blockId2, style1 = 0, style2 = 0,
                              playerId = NULL, position = 0,
                              debugFlag = FALSE)
{
   drawDonut (r1, r2, blockId1, style = style1, playerId = playerId, 
              position = position, debugFlag)
   drawDonut (r1, r3, blockId2, style = style2, playerId = playerId, position = position,
              debugFlag)
}

# R is outer radius, r is inner radius

# Code ported by Felix Ling from Python code by Alexander Pruss and under the MIT license

drawDonut <- function (R, r, blockId = GLASS, style = 0, position = 0,
                       playerId = NULL, debugFlag = FALSE)
{
   # Get current player position and heading.
   Pos <- getPlayerPos (playerId, tile = TRUE)
   mcx <- Pos[1]
   mcy <- Pos[2]
   mcz <- Pos[3]

   if (debugFlag) print (paste ("mcx:", mcx, "mcyy:", mcy, "mcz:", mcz))
   
   # if (debugFlag) print (paste ("x range:", , "mcyy:", mcy, "mcz:", mcz))
   
   for (x in ((-R - r):(R + r))) {
      for (y in ((-R - r):(R + r))) {
         xy_dist <- sqrt (x**2 + y**2)
         
         if (xy_dist > 0) {
            ringx <- x / xy_dist * R # nearest point on major ring
            ringy <- y / xy_dist * R
            ring_dist_sq <- (x-ringx)**2 + (y-ringy)**2
            
            for (z in ((-R-r):(R+r))) {
               # if (debugFlag) print (paste ("x:", x, "y:", y, "z:", z))
               
               if (ring_dist_sq + z**2 <= r**2) 
               {
                  # if (debugFlag) print (paste ("x:", x, "y:", y, "z:", z))
                  if (debugFlag) print ("SETTING BLOCK!")

                  if (position == 0) {
                     setBlock (mcx + x, mcy + z, mcz + y, blockId, style)
                  } else if (position == 1) {
                     # Flip y and z
                     setBlock (mcx + x, mcy + y, mcz + z, blockId, style)
                  } else if (position == 2) {
                     # Flip x and z
                     setBlock (mcx + z, mcy + y, mcz + x, blockId, style)
                  }
               }
            }
         }
      }
   }
}


# Code by Alexander Pruss and under the MIT license

# Getting
# Error in done[x + r + 1, y + r + 1, z + r + 1] : subscript out of bounds
# Is done necessary? program resets it before each one. No, it preserves it 
# in each loop! Oh no, this won't work!

ball <- function (x0, y0, z0, r, block_type, debugFlag = TRUE)
{
   if (debugFlag) print (paste ("done sz:", (2 * r)^3, "len:", (2 * r)))
   done <- array (0, c (2 * r + 1, 2 * r + 1, 2 * r + 1))
   
   for (x in (-r:r)) {
      for (y in (-r:r)) {
         for (z in (-r:r)) {
            if (x^2 + y^2 + z^2 <= r^2) {
               if (debugFlag) print (paste ("checking done at", x + r, y + r, z + r))
               # Done is a set, which in Python is unordered with no unique
               # elements. Might be better to have a parallel 3D array to mark
               # it there?
               if (done[x + r + 1, y + r + 1, z + r + 1] == 0) {
                  if (debugFlag) print (paste ("setting at", x0 + x, y0 + y, z0 + z))
                  setBlock (x0+x, y0+y, z0+z, block_type)
                  done[x + r + 1, y + r + 1, z + r + 1] <- 1
               }
            }
         }
      }
   }
   return (done)
}


# Shows what kinds of blocks you'll be able to find from a mine.
mineBarplot <- function (length = 30, height = 3, playerId = NULL) 
{
   pos <- getPlayerPos (tile = TRUE)
   
   # Read the block IDs in a 30x3x30 slice of the mine.
   # Mine <- getBlocks (-30, -54, -37, 0, -51, -7)
   mine <- getBlocks (pos[1] - length / 2, pos[2], pos[3] - length / 2, 
                      pos[1] + length / 2, pos[2] + height - 1, 
                      pos[3] + length / 2)
   
   # There were over 1,000 stone blocks. Filter them out! Should also filter out
   # air, dirt, and torches! 
   # MineNoStone <- Mine[Mine != STONE]
   mineNoStone <- mine[!(mine %in% c (AIR, STONE, DIRT, TORCH))]
   
   # We're mostly interested in the counts, so convert the 3D array to a 2-way
   # table.
   countsNoStone = table (mineNoStone)
   
   # This plots the counts vs. the block IDs.
   barplot (countsNoStone, col = "blue")
   
   # Manually substitute the names. 

   # dimnames (countsNoStone) <- list (c ("cobblestone", "water", "gravel", "gold ore",
   #                                      "iron ore", "coal ore",
   #                                      "lapis lazuli ore", 
   #                                      "diamond ore","redstone ore"))

   # dimnames (countsNoStone) <- list (c ("cobblestone", "gravel",
   #                                      "iron ore", "coal ore",
   #                                      "lapis lazuli ore",
   #                                      "diamond ore","redstone ore"))
   
   
   # This plots the counts vs. the block names.   
   barplot (countsNoStone, col = "blue", main = paste (length, "block mine"))
}

hollowMountain <- function (playerId = NULL, hardLimit = 50000, debugFlag = TRUE)
{
   # Find out where the player is looking.
   if (is.null (direction))
      direction <- getPlayerCompassDir ()
   
   if (is.null (x) || is.null (y) || is.null (z)) pos <- getPlayerPos (playerId, tile = TRUE)
      
   # Move in the direction the player is looking, and up one in case there is
   # carpet
   x <- pos[1] + xInc
   y <- pos[2] + 1
   z <- pos[3] + zInc
   isNotAir <- getBlock (x, y, z, FALSE)
   
   # Scan in the direction the player is looking until a non-air block is
   # found.
   while (isNotAir == AIR) {
      x <- x + xInc
      z <- z + zInc
      isNotAir <- getBlock (x, y, z, FALSE)
      
      # Move back down to the player's feet/ground/carpet.
      y <- y - 1
   }

   # Get perimeter at player's height.
   
   # Loop from player's height up to peak(s).
   
   # Map result from getHeight to 3D array. All y's higher than this must be outside
   # the mountain.
   
   
   

}
