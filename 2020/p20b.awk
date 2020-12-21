#!/usr/bin/env awk -f

function print_map(    x, yy) {
    for (yy=0; yy <= tsize * sqrt(num_tiles); yy++) {
        if (yy % tsize == 0) print ""
        for (x=0; x < sqrt(num_tiles); x++) {
            printf "%s ", tiles[tilemap[x, int(yy / tsize)], 1 + (yy % tsize)]
        }
        print ""
    }
}

function print_stitched(    y) {
    for (y=1; y <= stitched_size; y++)
        print stitched[y]
}

function revstr(s,    r, i) {
    for (i=length(s); i >= 1; i--) r = r substr(s, i, 1)
    return r
}

function countchar(s, char,    i, r) {
    for (i=1; i <= length(s); i++)
        r += substr(s, i, 1) == char ? 1 : 0
    
    return r
}

# anywhere mask is "#", replace character in the string with char
function maskchar(s, mask, char,    i, r) {
    for (i=1; i <= length(mask); i++) {
        if (substr(mask, i, 1) != "#") {
            r = r substr(s, i, 1)
        } else {
            r = r char
        }
    }
    return r
}

#   0
# 3   1
#   2
function getedge(tilen, dir, rev,    t, i) {
    if (dir == 0) {
        t = tiles[tilen,1]
    }
    if (dir == 2) {
        t = tiles[tilen,tsize]
    }
    if (dir == 1) {
        for (i=1; i <= tsize; i++) t = t substr(tiles[tilen, i], tsize, 1)
    }
    if (dir == 3) {
        for (i=1; i <= tsize; i++) t = t substr(tiles[tilen, i], 1, 1) 
    }
    
    if (rev) {
        t = revstr(t)
    }
    
    return t
}

function flipH(tilen,    t, i) {
    for (i=1; i <= tsize / 2; i++) {
        t = tiles[tilen, i]
        tiles[tilen, i] = tiles[tilen, tsize - i + 1]
        tiles[tilen, tsize - i + 1] = t
    }
}

function flipHStitched(    t, i) {
    for (i=1; i <= stitched_size/2; i++) {
        t = stitched[i]
        stitched[i] = stitched[stitched_size - i + 1]
        stitched[stitched_size - i + 1] = t
    }
}

function flipV(tilen,    i) {
    for (i=1; i <= tsize; i++) {
        tiles[tilen, i] = revstr(tiles[tilen, i])
    }
}

function flipVStitched(    i) {
    for (i=1; i <= stitched_size; i++)
        stitched[i] = revstr(stitched[i])
}

function rottile(tilen,    r, x, j) {
    for (x=1; x <= tsize; x++) {
        for (j=tsize; j >= 1; j--) {
            r[x] = r[x] substr(tiles[tilen,j], x, 1)
        }
    }
    for (x=1; x <= tsize; x++) {
        tiles[tilen, x] = r[x]
    }
}

function rotStiched(    r, x, j) {
    for (x=1; x <= stitched_size; x++)
        for (j=stitched_size; j >= 1; j--)
            r[x] = r[x] substr(stitched[j], x, 1)
    
    for (x=1; x <= stitched_size; x++)
        stitched[x] = r[x]
}

# Find tile which has an edge (with any rot/flip) that is equal to target ("#...#.")
# Exclude target_tile ID
# Returns true if one is found
function find_edge(target_tile, target,    i, e, dir) {
    for (i=1; i <= num_tiles; i++) {
        if (tilenums[i] != target_tile) {
            for (e=0; e <= 3; e++) {
                for (dir=0; dir <= 1; dir++) {
                    if (getedge(tilenums[i], e, dir) == target)
                        return 1
                }
            }
        }
    }
}

# Find tile that has target_edge (0-3) edge equal to str target_edge_val, after rot/flip
# Modifies tiles in-place, and leaves the matching one in the right orientation
# Returns tileID of the matching tile
function find_for_pos(target_edge_val, target_edge,    i, tn, numrots, numhflips, numvflips) {
    for (i=1; i <= num_tiles; i++) {
        tn = tilenums[i]
        if (tn in anchored) continue;
        
        for (numrots=0; numrots <= 3; numrots++) {
            for (numhflips=0; numhflips <= 1; numhflips++) {
                for (numvflips=0; numvflips <= 1; numvflips++) {
                    if (getedge(tn, target_edge, 0) == target_edge_val) {
                        return tn;
                    }
                    flipV(tn)
                }
                
                flipH(tn)
            }
        
            rottile(tn)
        }
    }
}

# Starting with anchored tile at (x, y), anchor all others recursively
# This is very brute-forced.
function populate(x, y,    e, next_tile) {
    for (e=0; e <= 3; e++) {
        next_tile = find_for_pos(getedge(tilemap[x,y], e, 0), (e + 2) % 4)
        if (next_tile) {
            anchored[next_tile] = 1
            if (e == 0) {
                tilemap[x, y-1] = next_tile
                populate(x, y-1)
            }
            if (e == 1) {
                tilemap[x+1, y] = next_tile
                populate(x+1, y)
            }
            if (e == 2) {
                tilemap[x, y+1] = next_tile
                populate(x, y+1)
            }
            if (e == 3) {
                tilemap[x-1, y] = next_tile
                populate(x-1, y)
            }
        }
    }
}

function stitch_map(    tx, ty, y, row) {
    stitched_size = 0
    for (ty=0; ty < sqrt(num_tiles); ty++) {
        for (y=2; y < tsize; y++) {
            row = ""
            for (tx=0; tx <= sqrt(num_tiles); tx++) {
                row = row substr(tiles[tilemap[tx, ty],y], 2, tsize-2)
            }
            stitched[++stitched_size] = row
        }
    }
}

# coord is 1-based
function is_monster(y, x,    t, t2, t3, res, monster_len, monsterpat, i) {
    monsterpat[1] = "..................#."
    monsterpat[2] = "#....##....##....###"
    monsterpat[3] = ".#..#..#..#..#..#..."
    monster_len = length(monsterpat[1])
    for (x=1; x <= (length(stitched[y]) - monster_len + 1); x++) {
        if (substr(stitched[y    ], x, monster_len) ~ monsterpat[1] &&
            substr(stitched[y + 1], x, monster_len) ~ monsterpat[2] &&
            substr(stitched[y + 2], x, monster_len) ~ monsterpat[3]) {
            
            res += 1
            
            # If one monster found, assume won't need anymore rotations or anything, so it's fine to trash the map
            for (i=0; i <= 2; i++) {
                stitched[y+i] = substr(stitched[y+i], 1, x-1) maskchar(substr(stitched[y+i], x, monster_len), monsterpat[i+1], "O") substr(stitched[y+i], x + monster_len)
            }
        }
    }
    
    return res + 0
}

# Find all monsters on the map and return their count
function do_monster(    y, res, i) {
    res = 0
    for (y=1; y <= (stitched_size - 3); y++) {
        for (i=1; i <= 4; i++)
            res += is_monster(y)
    }
    
    return res
}

BEGIN {
    # Each tile goes into a record
    RS = "\n\n"
    # Each line goes into a field
    FS = "\n"
    # Tiles are always 10 square
    tsize = 10
}

{
    
    sub(/Tile /, "", $1)
    tilen = $1 + 0
    
    for (i=2; i <= (tsize + 1); i++) {
        tiles[tilen, i-1] = $i
    }
    tilenums[++num_tiles] = tilen
}

END {
    # Find a tile with edges 0 and 3 unmatched and anchor it in (0,0)
    for (i=1; i <= num_tiles && !found_tile; i++) {
        tn = tilenums[i]
        for (numrots=0; numrots <= 3 && !found_tile; numrots++) {
            for (numhflips=0; numhflips <= 1 && !found_tile; numhflips++) {
                for (numvflips=0; numvflips <= 1 && !found_tile; numvflips++) {
                    if (!find_edge(tn, getedge(tn, 0, 0)) &&
                        !find_edge(tn, getedge(tn, 3, 0))) {
                            found_tile = tn
                    }
                    else
                        flipV(tn)
                }
                
                if (!found_tile) flipH(tn);
            }
            
            if (!found_tile) rottile(tn)
        }
    }
    
    tilemap[0,0] = found_tile
    anchored[found_tile] = 1
    
    # Place the rest of tiles
    populate(0, 0)
    
    # Stich into a single stitched
    stitch_map()
    
    # Tests for rot/flip of stitched
    
    #print "    FLIP H"
    #flipHStitched()
    #for (y=1; y <= stitched_size; y++)
        #print stitched[y]
    
    #print "flip V"
    #flipVStitched()
    #for (y=1; y <= stitched_size; y++)
        #print stitched[y]
        
    #print "rotate"
    #rotStiched()
    #for (y=1; y <= stitched_size; y++)
        #print stitched[y]
    
    # Rot/flip stitched until any monsters are found.
    monster_res = 0
    for (numrots=0; numrots <= 3 && !monster_res; numrots++) {
        for (numhflips=0; numhflips <= 1 && !monster_res; numhflips++) {
            for (numvflips=0; numvflips <= 1 && !monster_res; numvflips++) {
                print "==="
                print_stitched()

                monster_res = do_monster()
                if (monster_res) {
                    print "found"
                    print_stitched()
                    
                    result = 0
                    for (y=1; y<= stitched_size; y++)
                        result += countchar(stitched[y], "#")
                    
                    print result
                } else
                    flipVStitched()
            }
            
            if (!monster_res) flipHStitched()
        }
        
        if (!monster_res) rotStiched()
    }
}