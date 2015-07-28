open System


 // Node type for storing a trie.  
 type TrieNode =   
   | Node of char * TrieNode list * bool  
   | Root of TrieNode list 

// Basic check for equality on a node against a character.  
 let matchChar c = (fun node -> match node with  
                 | Root(_) -> false  
                 | Node(v, _, _) -> v = c)  
    
 // Insert a new node into the trie.  
 let rec Insert (chars: char list) = function  
   // If this is the root node and it starts on a known character continue, otherwise add a new child, then continue.  
   | Root(list) ->  if List.exists(matchChar chars.Head) list = false  
                        then Root(list @ [(Node(chars.Head, [], false) |> Insert chars)])  
                    else Root(List.map (fun node -> Insert chars node) list)  
   // If we've ran out letters it's now a word. If there's no children, make some, as we have more letters.  
   | Node(c, list, isword) when c = chars.Head ->   if chars.Tail = []   
                                                        then Node(c, list, true)  
                                                    else if list = []   
                                                        then Node(c, [(Node(chars.Tail.Head, [], false) |> Insert chars.Tail)], isword)  
                                                        // If we need to add a child to continue, do so.  
                                                    else if List.exists(matchChar chars.Tail.Head) list = false   
                                                        then Node(c, list @ [(Node(chars.Tail.Head, [], false) |> Insert chars.Tail)], isword)  
                                                    // Continue down the tree.  
                                                    else Node (c, List.map (fun node -> Insert chars.Tail node) list, isword)  
   // Default case for and existing node.  
   | Node(c, list, isword) -> Node(c, list, isword)


let Exists (word: string, tree: TrieNode) =   
   // Turn the string into a list of char.  
   let wordChars = [for character in word -> character]  
    
   // Look through the tree for the word.  
   let rec find (chars: char list) = function  
     // If it's the root, look at all the child nodes.  
     | Root(list) -> if list = []   
                        then false  
                     else list |> List.exists (fun node -> find chars node)  
     // If we've found the last character, is it a word? If there's no more tree, it's not a word, otherwise keep searching.  
     | Node(c, list, isword) when c = chars.Head -> if chars.Tail = []  
                                                        then isword  
                                                    else if list = []  
                                                        then false  
                                                    else list |> List.exists (fun node -> find chars.Tail node)  
     | Node(c, list, isword) -> false  
    
   // Find the word.  
   find wordChars tree 


let root = Root([]);  
let b = Insert ['a';'b'] root  
let c = Insert ['b';'c'] b  
let d = Insert ['a';'b';'c'] c  
let e = Insert ['a';'b';'d'] d  
let f = Insert ['a';'b';'c';'d';'e';'f';'g';'h'] e  
let exists = Exists ("abd", f)  