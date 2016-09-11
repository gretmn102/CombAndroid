module CombChars

open System

module word =
    type WordSig = WordSig of (char*int) list
    let extract word = 
        word
        |> Seq.countBy id
        |> Seq.sort
        |> List.ofSeq
        |> WordSig

    let extracts wordList =
        wordList
        |> Seq.map (fun x -> x, extract x)
        |> Seq.groupBy snd
        |> Seq.map (fun (s, words) -> s, words |> Seq.map fst |> List.ofSeq)

    let extractsExcept wordList exceptChars =
        let filter str = 
            let f char = Seq.filter (fun x -> x <> char)
            exceptChars |> Seq.fold (fun state x -> f x state) str
        wordList
        |> Seq.map (fun x -> x, filter x |> extract)
        |> Seq.groupBy snd
        |> Seq.map (fun (s, words) -> s, words |> Seq.map fst |> List.ofSeq)

module comb = 
    open word
    type 'a Pair = P of 'a * 'a Pair list
    let UnionsWithoutRepetitions k L =
        let map f k L =
            let rec mapr k = function
                | [] -> []
                | car::cdr ->
                    if k = 0 then
                        [(f car cdr)]
                    else
                        (f car cdr)::mapr (k-1) cdr
            mapr k L
        let rec f i L =
            if i >= 0 then
                map (fun x l -> P(x, f (i-1) l)) (List.length L - 1 - i) L
            else
                []
        // распаковать в вид [[1;2]; [1;3]...]
        let insert pairs =
            let f e xs = 
                List.map (fun x -> e::x) (List.concat xs)
            let rec f2 = function
                | P(e, []) -> [[e]]
                | P(e, L) -> f e (List.map f2 L)
            List.concat <| List.map f2 pairs

        f (k-1) L
        |> insert
    let comb length chars = 
        let xs =
            UnionsWithoutRepetitions length (List.ofSeq chars)
            |> Seq.map extract
        if chars |> Seq.groupBy id |> Seq.length = Seq.length chars then xs
        else
            let uniq = Seq.distinct xs
            //if List.ofSeq uniq = List.ofSeq g then failwith "uniq = g"
            uniq
///<summary>Все для комбинирования слов из входных букв</summary>
module withoutStar =
    open word
    type WordsBySig (s:WordSig, words:string list) =
        member __.Sig = s
        member __.Words = words

        override __.Equals(o : obj) =
            match o with
            | :? WordsBySig as other -> (other.Sig = s)
            | _ -> false
        override __.GetHashCode() = s.GetHashCode()
    
    let possibleWords (dicWords:seq<WordsBySig>) (gen:seq<WordsBySig>) = 
        let xs = Linq.Enumerable.Intersect(dicWords, gen)
        xs |> Seq.map (fun x -> x.Sig, x.Words)
    let convert words =
        words |> Seq.map(fun (s, ws) -> WordsBySig(s, ws))
    let convertWS ws =
        ws |> Seq.map (fun x -> WordsBySig(x, [""]))

module starProblem = 
    module binTree = 
        type BinTree<'key, 'value> =
            | Nil
            | Node of ('key * 'value) * (BinTree<'key, 'value> * BinTree<'key, 'value>)

        let rec insert (k, v) = function
            | Nil -> Node((k, v), (Nil, Nil))
            | Node((k2, v2), (left, right)) ->
                if k > k2 then Node((k2, v2), (left, insert (k,v) right))
                else if k < k2 then Node((k2, v2), (insert (k,v) left, right))
                else failwith "k = k2"
                //else Node((k2, v), (left

        let rec print = function
            | Nil -> "nil"
            | Node((k, _), (left, right)) ->
                let key = sprintf "%A" k
                key + "(" + print left + ", " + print right + ")"
            
    module avlTree =
        type Tree<'key, 'value> = 
            | Leaf
            | Node of int * Tree<'key, 'value> * ('key * 'value) * Tree<'key, 'value>
         
        let height = function Leaf -> -1 | Node(h, _, _, _) -> h
        let depth a b = 1 + (max (height a) (height b))

        let rec max' = function
            | Leaf -> None
            | Node(_, _, v, Leaf) -> Some v
            | Node(_, _, _, right) -> max' right
        (*
        rotate :: Tree a -> Tree a
        rotate Leaf = Leaf
        -- left left case
        rotate (Node h (Node lh ll lv lr) v r)
            | lh - height r > 1 && height ll - height lr > 0 = 
              Node lh ll lv (Node (depth r lr) lr v r)
        -- right right case
        rotate (Node h l v (Node rh rl rv rr))
            | rh - height l > 1 && height rr - height rl > 0 =
              Node rh (Node (depth l rl) l v rl) rv rr
        -- left right case
        rotate (Node h (Node lh ll lv (Node rh rl rv rr)) v r)
            | lh - height r > 1 = Node h (Node (rh+1) (Node (lh-1) ll lv rl) rv rr) v r
        -- right left case
        rotate (Node h l v (Node rh (Node lh ll lv lr) rv rr))
            | rh - height l > 1 = Node h l v (Node (lh+1) ll lv (Node (rh-1) lr rv rr))
        -- re-weighting
        rotate (Node h l v r) = let (l', r') = (rotate l, rotate r)
                                in Node (depth l' r') l' v r' *)
        let rec rotate = function
            | Leaf -> Leaf
            | Node(_, Node(lh, ll, lv, lr), v, r) when 
                lh - height r > 1 && height ll - height lr > 0 -> Node(lh, ll, lv, Node((depth r lr), lr, v, r))
            | Node(_, l, v, Node(rh, rl, rv, rr)) when
                rh - height l > 1 && height rr - height rl > 0 -> Node(rh, Node((depth l rl), l, v, rl), rv, rr)
            | Node(h, Node(lh, ll, lv, Node(rh, rl, rv, rr)), v, r) when
                lh - height r > 1 -> Node(h, Node((rh+1), Node((lh-1), ll, lv, rl), rv, rr), v, r)
            | Node(h, l, v, Node(rh, Node(lh, ll, lv, lr), rv, rr)) when
                rh - height l > 1 -> Node(h, l, v, Node((lh+1), ll, lv, Node((rh-1), lr, rv, rr)))
            | Node(_, l, v, r) -> let (l', r') = (rotate l, rotate r) in Node((depth l' r'), l', v, r')
        (*
       insert :: Ord a => a -> Tree a -> Tree a
        insert v Leaf = Node 1 Leaf v Leaf
        insert v t@(Node n left v' right)
            | v' < v = rotate $ Node n left v' (insert v right)
            | v' > v = rotate $ Node n (insert v left) v' right
            | otherwise = t *)
        let rec insert ((k, _) as x) = function
            | Leaf -> Node(1, Leaf, x, Leaf)
            | Node(n, left, ((k', _) as y), right) ->
                if k' < k then rotate <| Node(n, left, y, (insert x right))
                else if k' > k then rotate <| Node(n, (insert x left), y, right)
                else rotate <| Node(n, left, x, right)
        let rec insertUnbalance ((k, _) as x) = 
            let rec f = function
            | Leaf -> Node(1, Leaf, x, Leaf)
            | Node(n, left, ((k', _) as y), right) ->
                if k' < k then Node(n, left, y, (insertUnbalance x right))
                else if k' > k then Node(n, (insertUnbalance x left), y, right)
                else failwith "k = k2"
            f
            //let rec f' (Node(n, left, y, Leaf)) = function
            //    | Leaf -> 
        let find k = 
            let rec f last = function
            | Leaf -> last
            | Node(_, left, (x, v), right) ->
                if   k = x then (Some v)
                elif k > x then f (Some v) right
                else f (Some v) left
            f None

        let foldTree xs = Seq.fold (fun state x -> insert x state) Leaf xs

        let draw t = 
            let rec draw' d = function
            | Leaf -> ""
            | Node(h, l, v, r) -> 
                let padding n = String.replicate (n*4) " "
                let node = padding d + (sprintf "(%A, %d)" v h) + "\n"
                draw' (d+1) r + node + draw' (d+1) l
            "\n" + draw' 0 t + "\n"
    module avlTreeM =
        type Tree<'key, 'value> = 
            | Leaf
            | Node of int * Tree<'key, 'value> * ('key * 'value) * Tree<'key, 'value>
        type Var<'key, 'value> =
            | Leaf
            | Node of Node<'key, 'value>
        and Node<'key, 'value> = 
            { mutable Height: int
              Value: 'key * 'value
              mutable Left: Var<'key, 'value>
              mutable Right:Var<'key, 'value> }

        let height = function Leaf -> -1 | Node({ Height = h }) -> h
        let depth a b = 1 + (max (height a) (height b))

        let rec max' = function
            | Leaf -> None
            | Node({ Value = v; Right = Leaf}) -> Some v
            | Node({Right = r}) -> max' r

        let rec rotate = function
            | Leaf -> Leaf
            | Node({Left = Node({Height = lh; Left = ll; Right = lr} as node'); Right = r} as node) when 
                lh - height r > 1 && height ll - height lr > 0 -> 
                    Node{node' with Right = Node{ node with Height = (depth r lr); Left = lr}}
            | Node({Left = l; Right = Node({Height = rh; Left =  rl; Right = rr} as node')} as node) when
                rh - height l > 1 && height rr - height rl > 0 -> 
                    Node{node' with Left = Node{node with Height = (depth l rl); Right =  rl};}
            | Node({Right = r; Left = Node({Height = lh; Right = Node({Height = rh; Left = rl} as rnode)} as lnode)} as node) when 
                lh - height r > 1 ->
                    Node{node with Left = Node{rnode with Height = rh+1; 
                                                          Left = Node{lnode with Height = lh - 1;
                                                                                 Right = rl}}}
            | Node({Left = l; Right = Node({Height = rh; Left = Node({Height = lh; Left = lr} as lnode)} as rnode)} as node) when 
                rh - height l > 1 ->
                    Node{node with Right = Node{lnode with Height = lh+1; 
                                                           Right = Node{rnode with Height = rh - 1;
                                                                                  Left = lr}}}
            | Node({Right = r; Value = v; Left = l}) -> let (l', r') = (rotate l, rotate r) in Node({Height = (depth l' r'); Left = l'; Value = v; Right = r'})

        let rec rotate2 = function
            | Leaf -> Leaf
            | Node({Left = Node({Height = lh; Left = ll; Right = lr} as node'); Right = r} as node) when 
                lh - height r > 1 && height ll - height lr > 0 -> 
                    node.Height <- (depth r lr)
                    node.Left <-lr
                    node'.Right <- Node node
                    Node node'
            | Node({Left = l; Right = Node({Height = rh; Left =  rl; Right = rr} as node')} as node) when
                rh - height l > 1 && height rr - height rl > 0 -> 
                    node.Height <- (depth l rl)
                    node.Right <- rl
                    node'.Left <- Node node
                    Node node'

            | Node({Right = r; Left = Node({Height = lh; Right = Node({Height = rh; Left = rl} as rnode)} as lnode)} as node) when 
                lh - height r > 1 -> 
                    lnode.Right <- rl
                    lnode.Height <- lh - 1
                    rnode.Left <- Node lnode
                    rnode.Height <- rh+1
                    node.Left <- Node rnode
                    Node node

            | Node({Left = l; Right = Node({Height = rh; Left = Node({Height = lh; Left = lr} as lnode)} as rnode)} as node) when 
                rh - height l > 1 -> 
                    rnode.Left <- lr
                    rnode.Height <- rh - 1
                    lnode.Right <- Node rnode
                    lnode.Height <- lh+1
                    node.Right <- Node lnode
                    Node node
            | Node({Right = r; Left = l} as node) -> 
                let (l', r') = (rotate l, rotate r)
                node.Height <- depth l' r'
                node.Left <- l'
                node.Right <- r'
                Node node
                

        let insert ((k, _) as x) =
            let empty = Node({Height = 1; Left = Leaf; Value = x; Right = Leaf})
            let rec f = function
            | Leaf -> empty
            | Node({Left = l; Value = (k', _); Right = r} as node) ->
                if k' < k then rotate <| Node{node with Right = f r}
                else if k' > k then rotate <| Node{node with Left = f l}
                else rotate <| Node{node with Value = x}
            f

        let foldTree xs = Seq.fold (fun state x -> insert x state) Leaf xs
        let insertUnbalance ((k, _) as x) t = 
            let empty = Node({Height = 1; Left = Leaf; Value = x; Right = Leaf})
            let rec f = function
            | Leaf -> ()
            | Node({Left = l; Value = (k', _); Right = r} as node) ->
                if k' < k then
                    match r with
                    | Leaf -> node.Right <- empty;
                    | x -> f x
                else if k' > k then
                    match l with
                    | Leaf -> node.Left <- empty;
                    | x -> f x
                else failwith "k = k'"

            match t with
            | Leaf -> empty
            | t -> f t; t

        let foldUnbalance xs = Seq.fold (fun state x -> insertUnbalance x state) Var.Leaf xs
        let find k = 
            let rec f last = function
            | Leaf -> last
            | Node({Left = left; Value = (x, v); Right = right}) ->
                if k = x then (Some v)
                else if k > x then f (Some v) right
                else f (Some v) left
            f None
        module rebalance =
            let rebalance tree = 
                let map f tree = 
                    let rec iter = function
                    | Leaf -> Leaf
                    | Node({Left = l; Right = r} as node) ->
                        f (Node{node with Left = iter l; Right = iter r })
                    iter tree
                map rotate tree |> rotate

        let draw t = 
            let rec draw' d = function
            | Leaf -> ""
            | Node({Height = h; Left = l; Value = v; Right = r}) -> 
                let padding n = String.replicate (n*4) " "
                let node = padding d + (sprintf "(%A, %d)" v h) + "\n"
                draw' (d+1) r + node + draw' (d+1) l
            "\n" + draw' 0 t + "\n"
    
    let binarySearch value xs =
        let rec binarySearch(low, high) =
            if (high < low) then (low, high)
            else
                let mid = (low + high) / 2
                
                if (List.nth xs mid |> fst > value) then
                    binarySearch (low, mid-1)
                else if (List.nth xs mid |> fst < value) then
                    binarySearch (mid+1, high)
                else
                    (-1, mid)
        binarySearch(0, List.length xs)
    let sanbox() = 
        (*k
        let tree = 
            avlTree.foldTree [1..40..9999] |> ignore
        let lst = List.map (fun x -> x, x) [1..40..9999]
        Map.ofList lst *)
        let inline f x = x, string x |> ((+)"str")
        let tree = avlTree.foldTree ( List.map f [1..20] )
        //let tree = avlTree.insert (2, "notstr2") tree
        //avlTree.fi
        //WordSig(['a', 1]).GetHashCode()
        //WordSig(['a', 2]).GetHashCode()

        (set['a', 2; 'b', 10]).GetHashCode()

        