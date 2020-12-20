#!/usr/bin/env awk -f

function print_all_possible_edges(    i, j) {
    for (i in tilenums) {
        i = tilenums[i]
        
        for (j = 0; j <= 3; j++) {
            print getedge(i, j, 0)
            print getedge(i, j, 1)
        }
    }
}

function print_map(    x, yy) {
    for (yy=0; yy <= tsize * sqrt(num_tiles); yy++) {
        if (yy % tsize == 0) print ""
        for (x=0; x < sqrt(num_tiles); x++) {
            printf "%s ", tiles[tilemap[x, int(yy / tsize)], 1 + (yy % tsize)]
        }
        print ""
    }

}

function revstr(s,    r, i) {
    for (i=length(s); i >= 1; i--) r = r substr(s, i, 1)
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

function flipV(tilen,    i) {
    for (i=1; i <= tsize; i++) {
        tiles[tilen, i] = revstr(tiles[tilen, i])
    }
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
    # Print edges and check if edge combinations are unique (they are)
    # ./p20a.awk input/p20.txt  | sort | uniq -c | awk '{print $1}' | sort | uniq
    # print_all_possible_edges()
    
    # Find a tile with edges 0 and 3 unmatched and anchor it in (0,0)
    # Brute-force: keep rot/flipping everything until a combination is found.
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
    
    populate(0, 0)

    # Print tile ID map
    #for (y=0; y < 3; y++) {
        #for (x=0; x < 3; x++)
            #printf "%s ", tilemap[x,y]
        #print ""
    #}
    
    print_map()
    
    mapxsize = sqrt(num_tiles) - 1
    
    print tilemap[0,0] * tilemap[0, mapxsize] * tilemap[mapxsize, 0] * tilemap[mapxsize, mapxsize]
    
    # Tests of tile operations
    #tn = tilenums[1]
    #for (i=1; i <= tsize; i++) print tiles[tn,i]
    #print "==="
    #rottile(tn)
    #for (i=1; i <= tsize; i++) print tiles[tn,i]
    
    # This is sufficient for part A, but not part B
    ## Find tiles that have 2 edges not matching any other edge
    #result = 1
    #for (i=1; i <= num_tiles; i++) {
        #matched_edges = 0
        #for (te = 0; te <= 3; te++) {
            #if (find_edge(tilenums[i], getedge(tilenums[i], te, 0))) {
                #matched_edges++;
            #}
        #}
        
        #if (matched_edges == 2) {
            #print "Found", tilenums[i]
            #result *= tilenums[i]
        #}
    #}
    #print result
}