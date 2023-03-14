
#=
  an implementation of Algorithm X with dancing links
=#

module AlgoX

export dlx_init, dlx_add_row, dlx_solve

mutable struct Node
    tag::String
    cnt::Int
    l::Node
    r::Node
    u::Node
    d::Node
    c::Node
    function Node()
        self = new()
        self.tag = "head"
        self.cnt = 0
        self.l = self.r = self.u = self.d = self.c = self
        return self
    end
end

mutable struct Origin
    head::Node
    cs::Vector{Node}
    ncol::Int
    nrow::Int
    answer::Vector{Vector{String}}
end

#=
  Overall view:
           :          :         :                   :
           |          |         |                   |
          u|         u|        u|                  u|
    +-----------r-+-------r-+-------r-+--     -+-------r-+  tag: "haed", "col_#"
 <---> head_node <-> col_1 <-> col_2 <-> .... <-> col_n <--->
    +-l-----------+-l-------+-l-------+--     -+-l-------+
           |d         |d        |d                  |d
           |          |         |                   |
           |         u| r      u| r                 |       tag: "1" or specified value
  <----------------> node <--> node <----....-------------->
           |        l |d      l |d                  |
           |          |         |                   |
           |          |        u| r                u| r     tag: "2" or specified value
  <--------------------------> node <----....----> node <-->
           |          |       l |d                l |d
           |          |         |                   |
           :          :         :                   :

               [c]ol_#
                 |                 l : node
            [u]p :                 r : node
               | |                 u : node
   [l]eft <-- node --> [r]ight     d : node
               |                   c : node
             [d]own


  Relation of Origin's properties (haed, cs and ncol) and Column nodes (header and column):

    head ---->
             |
        +----.------r-+-------r-+-------r-+--     -+-------r-+
      <--> head_node <-> col_1 <-> col_2 <-> .... <-> col_n <-->
        +l------------+-l-------+-l-------+--     -+-l-------+
      cs      [1]         [2]       [3]                [n+1]

    ncol = n
=#

function dlx_init(col_size::Int)::Origin
    @assert col_size > 0 "invalid column size"

    cs = Array{Node}(undef, col_size + 1)
    cs[1] = Node()
    cs[1].cnt = typemax(Int)    # sentinel
    for idx = 2:(col_size + 1)
        cs[idx] = Node()
        cs[idx].tag = "col_" * string(idx - 1)
        cs[idx].l = cs[idx - 1]
        cs[idx].r = cs[1]
        cs[idx - 1].r = cs[idx]
    end
    cs[1].l = cs[end]
    origin = Origin(cs[1], cs, col_size, 0, Array{Vector{String}}(undef, 0))

    return origin
end

function dlx_add_row(dlx::Origin, idx_lst::Vector{Int}, name::String = "")
    function hook_node(fst::Vector{Node}, prev::Vector{Node}, lst::Vector{Int})
        if length(lst) == 0
            return fst, prev
        end
        idx = lst[1]
        cell = Node()
        cell.tag = tag
        cell.u = dlx.cs[idx].u
        cell.u.d = cell
        cell.d = dlx.cs[idx]
        cell.d.u = cell
        cell.c = dlx.cs[idx]
        dlx.cs[idx].cnt += 1
        if length(prev) != 0
            prev[1].r = cell
            cell.l = prev[1]
            hook_node(fst, [cell], lst[2:end])
        else
            hook_node([cell], [cell], lst[2:end])
        end
    end

    s_lst = sort(map((x) -> x + 1, idx_lst))
    @assert s_lst[1] > 1 && s_lst[end] <= dlx.ncol + 1 "invalid index value"

    dlx.nrow += 1
    tag = (name == "") ? string(dlx.nrow) : name
    head, tail = hook_node(Array{Node}(undef, 0), Array{Node}(undef, 0), s_lst)
    if length(head) != 0
        h_cell, t_cell = head[1], tail[1]
        t_cell.r = h_cell
        h_cell.l = t_cell
    end
end

@enum Direction Left Right Up Down

function _follow(n::Node, f::Function, dir::Direction)
    function next(n::Node)
        if dir == Up
            return n.u
        elseif dir == Down
            return n.d
        elseif dir == Left
            return n.l
        else
            return n.r
        end
    end

    function loop(n::Node)
        if n != stop
            f(n)
            loop(next(n))
        end
    end

    stop = n
    loop(next(n))
end

function _dlx_cover(col_node::Node)
    function _cover(n::Node)
        n.u.d = n.d
        n.d.u = n.u
        n.c.cnt -= 1
    end
    _cover_row(n::Node) = _follow(n, _cover, Right)

    col_node.r.l = col_node.l
    col_node.l.r = col_node.r
    _follow(col_node, _cover_row, Down)
end

function _dlx_uncover(col_node::Node)
    function _uncover(n::Node)
        n.u.d = n
        n.d.u = n
        n.c.cnt += 1
    end
    _uncover_row(n::Node) = _follow(n, _uncover, Left)

    _follow(col_node, _uncover_row, Up)
    col_node.r.l = col_node
    col_node.l.r = col_node
end

function dlx_solve(dlx::Origin)::Vector{Vector{String}}
    function find_min(n::Node, stop::Node, result::Node)
        if n == stop
            return result
        end
        if n.cnt < result.cnt
            find_min(n.r, stop, n)
        else
            find_min(n.r, stop, result)
        end
    end

    function solve(dlx::Origin, acc::Vector{String})
        if dlx.head.r == dlx.head
            push!(dlx.answer, reverse(acc))
        else
            col_node = find_min(dlx.head.r, dlx.head, dlx.head.r)
            if col_node.cnt > 0
                _dlx_cover(col_node)
                function _iter(n::Node)
                    _follow(n, (x) -> _dlx_cover(x.c), Right)
                    solve(dlx, vcat([n.tag], acc))
                    _follow(n, (x) -> _dlx_uncover(x.c), Left)
                end
                _follow(col_node, _iter, Down)
                _dlx_uncover(col_node)
            end
        end
    end

    if length(dlx.answer) != 0
        empty!(dlx.answer)
    end
    solve(dlx, Array{String}(undef, 0))
    dlx.answer
end

end #module
