#!/usr/bin/env awk -f
BEGIN {
    mem_next = 1
    # memory_val[mem_next] = value of node
    # memory_next[mem_next] = memory index of next node
    # node_values[value] = memory_index of the node
}

function print_memory(    i) {
    for (i=1; i <= mem_next; i++)
        print (i ":"), memory_val[i]+0, "->", get_next(i)
}

function create_node(value) {
    memory_val[mem_next] = value
    memory_next[mem_next] = mem_next
    
    node_values[value] = mem_next
    
    mem_next++
    return mem_next - 1
}

function insert_after(nodeind, atind,    atnext) {
    atnext = get_next(atind)
    
    if (get_next(nodeind) != nodeind)
        print "ASSERT insert non-self node"
    
    memory_next[atind] = nodeind
    memory_next[nodeind] = atnext
}

function insert_three_after(nodeind, atind,    fnode, atnext) {
    fnode = get_next(get_next(nodeind))
    
    atnext = get_next(atind)
    
    memory_next[atind] = nodeind
    memory_next[fnode] = atnext
}

function get_next(nodeind) {
    return memory_next[nodeind]
}

function format_cups(pos, sep, limit,    c, r) {
    if (!sep) sep = " "
    if (!limit) limit = 20
    
    r = memory_val[pos]
    c = get_next(pos)
    limit--
    
    while (c != pos && limit > 0) {
        r = r sep memory_val[c]
        c = get_next(c)
        limit--
    }
    
    return r
}

function pick_three(pos,    pos1, pos2, pos3) {
    pos1 = get_next(pos)
    pos2 = get_next(pos1)
    pos3 = get_next(pos2)
    
    memory_next[pos] = get_next(pos3)
    memory_next[pos3] = pos1
    
    return pos1
}

function find_dest(target, exc1,    exc2, exc3, i) {
    exc2 = memory_val[get_next(exc1)]
    exc3 = memory_val[get_next(get_next(exc1))]
    exc1 = memory_val[exc1]
    
    for (i=1; i <= 10; i++) {
        target--
        if (target == 0) target = max_number
        if (target != exc1 && target != exc2 && target != exc3)
            return node_values[target]
    }
}

function do_move(move_num,    taken, destpos) {
    if (move_num % 100000 == 0) print "-- move", move_num, " ---"
    #print "-- move", move_num, " ---"
    #print "cups:", format_cups(cur_node)
    
    taken = pick_three(cur_node)
    
    #print "pick up:", format_cups(taken)
    
    #print "cups after taking", format_cups(cur_node)
    
    destpos = find_dest(memory_val[cur_node], taken)

    #print "destination", destpos, memory_val[destpos]
    
    insert_three_after(taken, destpos)
    cur_node = get_next(cur_node)
    
    #print "cur node", cur_node
    #print_memory()
}



{
    a = create_node(substr($0, 1, 1))
    cur_node = a
    max_number = substr($0, 1, 1)
    
    for (i=2; i <= length($0); i++) {
        n2 = create_node(substr($0, i, 1))
        insert_after(n2, a)
        a = n2
        
        max_number = substr($0, i, 1) > max_number ? substr($0, i, 1) : max_number
    }
}

END {
    #test()
    
    target_num = 1000000
    for (max_number=max_number + 1; max_number <= target_num; max_number++) {
        n2 = create_node(max_number)
        insert_after(n2, a)
        a = n2
    }
    max_number--
    
    #print format_cups(cur_node)
    #print_memory()
    
    num_moves =  10000000 
    for (i=1; i <= num_moves; i++) {
        do_move(i)
    }
    
    #print format_cups(cur_node, "\n")
    
    print memory_val[get_next(node_values[1])] * memory_val[get_next(get_next(node_values[1]))]
}