(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, s_list) = 
    case s_list of
        [] => NONE
      | x::xs => if same_string(s, x) 
                 then SOME xs
                 else
                    case all_except_option(s, xs) of
                      NONE => NONE
                    | SOME ls => SOME (x::ls)

fun get_substitutions1(list, s) = 
    case list of
        [] => []
      | x::xs => case all_except_option(s, x) of
                    NONE => get_substitutions1(xs, s)
                  | SOME ls => ls @ get_substitutions1(xs, s)

fun get_substitutions2(list, s) = 
    let fun helper(l, s, acc) = 
        case l of
            [] => acc
          | x::xs => case all_except_option(s, x) of
                        NONE => helper(xs, s, acc)
                      | SOME ls => helper(xs, s, acc @ ls)
    in
        helper(list, s, [])
    end

fun similar_names(list, {first=x, middle=y, last=z}) = 
    let 
        val subs = get_substitutions2(list, x);
        fun change_name (ls) = 
            case ls of
                [] => []
              | x'::xs => {first=x', middle=y, last=z}::change_name(xs)
    in
        {first=x, middle=y, last=z}::change_name(subs)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color c =
    case c of 
        (Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

fun card_value c = 
    case c of
        (_, Num i) => i
      | (_, Ace) => 11
      | _ => 10

fun remove_card (cs, c, e) = 
    case cs of
        [] => raise e
      | x::xs => if x = c 
                 then xs
                 else x::remove_card(xs, c, e)

fun all_same_color cs = 
    case cs of 
        [] => true
      | _::[] => true
      | head::(neck::tail) => (card_color(head) = card_color(neck) andalso all_same_color(neck::tail))

fun sum_cards cs = 
    let fun helper(cs, acc) = 
        case cs of 
            [] => acc
          | x::xs => helper(xs, card_value(x) + acc)
    in
        helper(cs, 0)
    end

fun score(cs, goal) = 
    let
        val sum = sum_cards(cs)
        val fac = if all_same_color(cs) then 2 else 1
        val score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        score div fac
    end

fun officiate(cs, ms, g) = 
    let
        fun helper (cards, moves, goal, helds) = 
            if sum_cards(helds) > goal
            then score(helds, goal)
            else
                case moves of 
                    [] => score(helds, goal)
                  | Draw::move_rest => (case cards of
                                            [] => score(helds, goal)
                                          | card_head::card_rest => helper(card_rest, move_rest, goal, card_head::helds))
                  | (Discard c)::move_rest => helper(cards, move_rest, goal, remove_card(helds, c, IllegalMove))
    in
        helper(cs, ms, g, [])
    end
