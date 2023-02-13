let dim = 3

type case = int * int * int

type couleur =
  | Vert
  | Jaune
  | Rouge
  | Noir
  | Bleu
  | Marron
  | Libre (*case libre du plateau*)
  | Dehors
  (*case en dehors du plateau, utile pour l'affichage*)
  | Nombre of int
  (*pour mettre des petits noms*)
  | Nom of string

let string_of_couleur x =
  match x with
  | Vert -> "Vert"
  | Jaune -> "Jaune"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Bleu -> "Bleu"
  | Marron -> "Marron"
  | Libre -> "Libre"
  | Dehors -> "Dehors"
  | Nombre n -> string_of_int n
  (*pour mettre des petits noms*)
  | Nom s -> s

type case_coloree = case * couleur

type configuration = case_coloree list * couleur list

type coup = Du of case * case | Sm of case list

let liste_joueurs (_, l) = l


(*fonction est_dans_losange qui prend une case et verifie si la case est dans le losange nord-sud selon la valeur de dim*)
let est_dans_losange = fun case -> match case with 
    (i,j,k) -> if (j >= (-dim) && j <= dim && k >= (-dim) && k <=dim) then true else false;;

(*fonction est_dans_etoile qui prend une case (i,j,k) et qui vérifie est dans l'étoile : 
      en remarquant que l'etoile se compose de trois losanges donc la fonction est_dans_etoile va vérifier si la case
      se trouve dans l'un des trois losanges donc renvoie vrai pour dire qu'elle est dans l'étoile, faux sinon
      les trois losanges sont nord-sud, (nord-est)-(sud-ouest) et (nord-ouest)-(sud-est)*)
let est_dans_etoile = fun case -> match case with 
    (i,j,k) -> if 
    (est_dans_losange (i,j,k) || 
     est_dans_losange(k,i,j) || est_dans_losange (j,k,i)) 
    && ((i+j+k)=0) then true else false ;;


(*fonction quelle_couleur qui prend une case et une configuration et retourne la couleur de la case si elle est dans 
  la configuration et si elle la trouve pas dans la liste des cases coloorées de la configuration elle regarde si elle 
  est dans l'étoile et retourne libre sinon dehors pour dire qu'elle est en dehors de l'étoile*)

let rec quelle_couleur = fun c -> fun config -> match config with
    (case_coloree_list, list_joueurs) -> 
    match case_coloree_list with
      [] -> if est_dans_etoile c then Libre else Dehors
    |e::q -> match e with (case,couleu) -> if case = c then couleu
      else quelle_couleur c (q,list_joueurs);; 



(*fonction tourne_case qui prend un entier et une case et qui deplace la case selon l'entier m et le nombre de joueurs 
  (voir fonction tourne_config) elle tourne la case de 1/6 à chaque fois : pour deplacer une case par exemple de camp
  camp sud-est en remarque qu'il suffit de changer les coordonnées (i,j,k) de la case à (-k,-i,-j) *)
let rec tourne_case = fun m -> fun case -> match case with 
    (i,j,k) -> if m = 1 then (-k,-i,-j) else tourne_case (m-1) (-k,-i,-j);;



(*fonction tourne_config qui prend en cofiguration et qui la tourne en faisant appel à la fonction tourne_case pour 
  deplacer toutes les cases coolorées de la configuration selon le nombre de joueurs (on remarque que si ya 6 joueurs il
  suffit de déplacer les cases de 1/6 mais s'il y a que 3 joueurs il faut déplacer les cases de 2*(1/6)) et à la fonction 
  tourne_couleur_list pour que le joueur n°1 prend la dernière place dans la liste après avoir jouer*)


let rec tourne_couleurs_list = fun list_joueurs -> fun i ->  match list_joueurs with
    [] -> [] 
  |e::q -> if i= 0 then e::q else (tourne_couleurs_list q (i-1))@[e];;

let rec tourne_config = fun configuration -> match configuration with 
    (case_coloree_list, list_joueurs) ->
    let rec tourne_case_list = fun liste_case -> fun list_joueurs -> match liste_case with 
        [] -> [] 
      |e::q -> match e with (case,couleur) -> match (List.length list_joueurs)
        with 

          6 -> ((tourne_case 1 case),couleur)::(tourne_case_list   q  list_joueurs)
        |3 -> ((tourne_case 2 case),couleur)::(tourne_case_list   q  list_joueurs)
        |2 -> ((tourne_case 3 case),couleur)::(tourne_case_list   q  list_joueurs)
        |_ -> liste_case
    in  
    ((tourne_case_list (case_coloree_list) list_joueurs), (tourne_couleurs_list (list_joueurs) 1));;


(*fonction sont_cases_voisines qui prend deux cases et vérifie si elles sont voisines : 
  on remarque que deux cases sont voisines si pour aller d'une à une autre il suffit de modifier les coordonnées 
  de 1 (exemple : on met +1 pour la coordonné j et -1 pour la coordonnée i) en tout y a 6 possibilités sont les cas 
  que la fontion teste *)
let sont_cases_voisines = fun case_1 -> fun case_2 -> match case_1 with
    (i,j,k) -> if (i+1,j-1,k)= case_2 || (i+1,j,k-1) = case_2 ||
                  (i-1,j,k+1)=case_2 ||(i-1,j+1,k) = case_2 ||(i,j+1,k-1)= case_2 
                  ||(i,j-1,k+1)=case_2 then true
    else false ;;    



(*fonction case_dans_config qui prend une case et une configuration : donc la fonction cherche dans la liste des 
  cases coolorées de la configuration si la case prise en paramètre est bien dans la liste*)
let rec case_dans_config = fun case -> fun configuration -> match configuration  with 
    (list_case_colorees, list_joueurs) -> 
    match list_case_colorees with 
      [] -> false
    |e::q -> match e with (cas_e,couleu) -> if case = cas_e then true else 
        case_dans_config case (q,list_joueurs);;



(*fonction remplir_triangle qui prend une configuration, une couleur et case et remplie un triangle d'une meme couleur.
  fonctionnement de la fonction : on remarque que remplir_tringle_aux prend les memes paramètre et une list de toutes les cases
  d'un triangle avec pointe vers le bas en case c, cette liste est construite en faisant appel à la fonction gerer_i_j 
  qui renvoie une liste d'un coté d'un triangle en jouant sur les coordonnées i et j (exemple : gerer_i_j (-6,3,3) = 
  [(-6,3,3);(-5,2,3);(-4,1,3)] ) et en faisant appel a la fonction gerer_j_k qui prend une liste génerer par gerer_i_j 
  et donne les cases qui sont à coté de chaque case de la liste gerer_i_j en allant de gauche à droite 
  (exemple : gerer_j_k (-4,1,3) = [(-4,1,3);(-4,2,2);(-4,3,1)]
  et enfin appel à la fonction list_list_case 
  (exemple : list_list_case (gerer_i_j)=[[(-6,3,3)];[(-5,2,3);(-5,3,2)];[(-4,1,3);(-4,2,2);(-4,3,1)]])  
  qui sera deconstruite par la fonction list_case_bis *)

  let rec remplir_triangle = fun conf -> fun col -> fun c -> 
  let  rec gerer_i = fun c -> fun x ->  match c with 
      (i,j,k) -> if x = 0 then [] else (i,j,k)::(gerer_i (i+1,j-1,k) (x-1))
  in let rec gerer_j_k = fun c ->  fun y -> match c with (i,j,k) -> 
      if y = 0 then [] else (i,j,k)::(gerer_j_k (i,j+1,(k-1)) (y-1)) 
  in let  rec list_case = fun list_gerer_i -> fun x ->  match list_gerer_i with
        [] -> []
      |e::q -> if x = dim+1 then [] else (gerer_j_k e x)::list_case q (x+1)
  in 

  let rec list_case_bis =  fun list_case -> match list_case with
      []-> []
    |e::b -> match e with 
        []-> list_case_bis b
      |a::[] -> a::list_case_bis b
      |a::q -> a::list_case_bis (q::b)


  in 
  let rec remplir_triangle_aux = fun conf -> fun col -> fun c -> fun list_case_bis ->
    match conf with (list_case_cooloree, list_joueurs) -> 
    match list_case_bis with 
      []-> conf
    |e::q -> remplir_triangle_aux (((e,col)::list_case_cooloree), list_joueurs) col c q 
  in 
  match c with (i,j,k) ->  remplir_triangle_aux conf col c (list_case_bis (list_case (gerer_i c (dim)) 1));;




(* fonction remplir_nit qui prend une liste de joueurs et construit à partir de cette liste une configuration initiale 
en remplissant pour chaque couleur ou joueur un triangle*)
let remplir_init = fun liste_joueurs -> 
  let rec remplir_init_aux = fun liste_joueurs -> fun conf -> 
    match liste_joueurs with
      [] -> conf 
    |e::q -> 
      remplir_init_aux q  (tourne_config(remplir_triangle conf e ((-2*dim),dim,dim))) in 

  remplir_init_aux liste_joueurs ([],liste_joueurs) ;;



let  configuration_initial = remplir_init [Vert; Jaune; Rouge; Noir; Bleu; Marron]


(*fonction est_dep_unit qui prend une configuration et deux cases et qui verfie si les déplacement des deux cases est unitaires
en vérifiant les couleurs des cases avec la fonction case_dans_config1 et si les deux cases sont voisines avec la fonction 
sont_voisines et si la case vers laquelle on veut le déplacement est bien dans le losange nord-sud 
si toutes les conditions sont satisfaite elle renvoie vrai*)
let rec case_dans_config1 = fun conf -> fun case -> match conf  with  
    (list_case_cooloree, liste_joueurs) -> 
    match list_case_cooloree with
      []-> Libre
    |e::q -> match e with (case1,couleur1) -> if case1 = case then couleur1 else 
        case_dans_config1 (q,liste_joueurs) case;;


let rec est_dep_unit = fun configuration -> fun case_1 -> fun case_2 -> 
  match configuration with (list_case_cooloree, list_joueurs) 
    -> match list_joueurs with 
      []-> false 
    |e::q -> if e = (case_dans_config1 configuration case_1) && (case_dans_config1 configuration case_2)= Libre
                &&  (est_dans_losange case_2) && (sont_cases_voisines case_1 case_2)  then  
        true else false ;; 


(*fonction fait_dep_unit qui prend une configuration et deux cases et change les couleur de ces deux cases 
la premiere case devient libre et la seconde devient une case cooloree avec la couleur de la premiere case en faisant *)

let rec fait_dep_unit = fun conf -> fun c1 -> fun c2 -> 
  let rec change_couleur_case = fun case -> fun list_case -> fun col -> 
    match list_case with
      [] -> list_case
    |e::q -> match e with (case1, couleur1) -> if case1 = case then(case1,col)::q else
        e::(change_couleur_case  case q col) 

  in match conf with (case_cooloree_list , list_joueurs)
    -> match list_joueurs with []-> conf 
                             |e::q -> 
                               ((change_couleur_case c1 ((c2,e)::(case_cooloree_list)) Libre), list_joueurs);;

(*fonction add_case qui additionne les coordonnees des deux cases qu'elle prend en parametres pour donner une nouvelle case *)
let add_case = fun case1 -> fun case2 -> match case1 with
    (i1,j1,k1) -> match case2 with 
       (i2,j2,k2) -> ((i1+i2),(j1+j2),(k1+k2));;
(*fonction diff_case qui calcule la difference entre les deux cases qu'elle prend en parametres pour retourner une nouvelle case*)
let diff_case = fun case1 -> fun case2 -> match case1 with
    (i1,j1,k1) -> match case2 with 
       (i2,j2,k2) -> ((i1-i2),(j1-j2),(k1-k2));;
(* fonction sont_alignees qui prend deux cases et retourne vrai ou faux en verifiant les deux cases si elles ont au moins l'une des coordonnees egale (sont alignees)*)
let sont_alignees = fun case1-> fun case2 -> match case1 with 
   (i1,j1,k1) -> match case2 with 
   (i2,j2,k2) -> if i1 = i2 || j1 = j2 || k1 = k2 then 
    true else false;;
(*fonction distance qui prend en parametre deux cases et retourne la distance entre ces deux dernieres 
en faisant la valeur absolu de la diffrence de l'une de leurs coordonnees qui n'est egale*)
let distance = fun case1 -> fun case2 -> match 
   case1 with (i1,j1,k1) -> 
    match case2 with (i2,j2,k2) -> if 
      i1 != i2 then abs(i2-i1) else if 
        j1 != j2 then abs(j2-j1) else if 
          k1 != k2 then abs(k2-k1) else 0;; 
(*fonction calcul_pivot qui prend en parametre deux cases et retourne le pivot entre elles (la case a michemin) 
en verifiant s'il existe deja *)
let calcul_pivot = fun case1 -> fun case2 -> 
     if (sont_alignees case1 case2) && ((distance case1 case2) mod 2)=0 then 
        match add_case case1 case2 with (i,j,k) -> 
          Some ((i/2),(j/2),(k/2))  
        else None;;

(*fonction vec_et_dist qui prend en parametre deux cases et retourne le vercteu de translation et la distance entre elles 
en dévisant le resultat de la difference des deux cases sur la distance*)
let vec_et_dist = fun case1 -> fun case2 -> match 
      case1 with (i1,j1,k1) -> 
        match case2 with 
        (i2,j2,k2) -> 
          ((((i1-i2)/distance case1 case2),((j1-j2)/distance case1 case2),
          ((k1-k2)/distance case1 case2)),distance case1 case2);;

(*fonction est_libre_seg qui prend en parametre deux cases et une cofiguration et retourne vrai si les cases situes entre les deux prise 
en parametre sont libres, faux sinon en verifiant a chaque fois la couleur de la case obtenu en faisant la difference entre la premiere case
et le vecteur de translation (case suivante) par la fonction case_dans_config1 *)
let est_libre_seg = fun case1 -> fun case2 -> fun config ->
  let rec est_libre_seg_aux = fun case1 -> fun case2 -> fun config -> fun i -> 
      match case1 with 
      (i1,j1,k1) -> match case2 with (i2,j2,k2) ->
        if i1=i2 && j1=j2 && k1=k2 then false
        else 
          match vec_et_dist case1 case2 with
          ((i3,j3,k3),n) -> if i = n || n = 1 then true 
          else if (case_dans_config1 config (diff_case case1 (i3*i,j3*i,k3*i))) = Libre then 
            est_libre_seg_aux case1 case2 config (i+1)
          else false

        in est_libre_seg_aux case1 case2 config 1;;

(* fonction est_saut qui prend en parametre deux cases et une configuration et retourne vrai 
si les deux case sont un saut valide dans la cofiguration en verfiant 
      - la premiere case doit etre de la meme couleur que le joueur a qui le tour de jouer
      - la deuxieme case doit etre Libre 
      - si y a un pivot de n'importe quelle couleur entre les deux cases et si les cases entre la premiere case et le pivot 
      et entre la deuxieme case et le pivot sont libres 
      , faux sinon  *)
let est_saut = fun case1 -> fun case2 -> fun config -> 
  match config with (list_case_cooloree, list_joueurs) 
    -> match list_joueurs with 
      []-> false 
    |e::q -> if 
  (case_dans_config1 config case2 ) = Libre && e = (case_dans_config1 config case1)
    then 
      match calcul_pivot case1 case2 with 
    None -> false 
    |Some x -> if (est_libre_seg case1 x config)  && (est_libre_seg  x case2 config) && case_dans_config1 config x != Libre
    then true else false 
else false;;

(*fonction fait_dep_saut_multiple qui prend une configuration et liste de cases d'un coup et retourne 
   une nouvelle configuration en effectuant le coup (changement de couleurs des cases) par la focntion change couleur*)
let fait_dep_saut_multiple = fun config -> fun list_case1 -> 
  let rec change_couleur_case = fun case -> fun list_case -> fun couleur -> 
    match list_case with
      [] -> list_case
    |e::q -> match e with (case1, couleur1) -> if case1 = case then(case1,couleur)::q else
        e::(change_couleur_case  case q couleur) 

  in match config with (case_cooloree_list , list_joueurs)
    -> if List.hd list_case1 = List.nth list_case1 ((List.length list_case1-1)) then config else  match list_joueurs with []-> config 
                             |e::q ->  
                     ((change_couleur_case (List.hd list_case1) (((List.nth list_case1 ((List.length list_case1)-1)),e)::(case_cooloree_list)) Libre), list_joueurs);;
(*fonction non_pivot qui prend en parametre une case et une liste de cases et verifie si le pivot 
de chaque deux cases concecutifs dans le liste n'est pas egale a la case prise en parametre *)
let rec non_pivot = fun case -> fun liste -> 
  match case with (i,j,k) -> 
  match liste with 
      [] -> true
      |e::[] -> true 
      |e::q::[] -> (match calcul_pivot e q with 
          None -> false 
          |Some (ip,jp,kp) -> if i=ip && jp=j && k=kp then false else true)
       |e::q -> match calcul_pivot e (List.hd q) with 
       None -> false 
          |Some (ip,jp,kp) -> if i=ip && jp=j && k=kp then false else
        non_pivot case q ;;

(* fonction est_saut_multiple qui prend une liste de cases et une configuration et qui verifie 
si la liste est un saut multiple valide en verifiant 
     - si chaque deux cases consecutifs sont un saut valide dans la config (avce la fonction est_saut)
     - la derniere case de la liste est bien dans le losange (avec la fonctio est_dans_losange)
     - si la premiere case (case de depart ) n'est pas utiliser comme pivots pour les sauts suivant (fonction non_pivot)*)


     let est_saut_multiple = fun list_case -> fun config -> 
  let rec est_multiple_aux = fun list_case -> fun config ->  
      match list_case with 
      [] -> false
      |e::[] -> false
      |e::q::[] -> est_saut e q config  
      |e::q -> if est_saut e (List.hd q) config && 
        est_dans_losange (List.nth list_case ((List.length list_case)-1)) then 
        est_multiple_aux q (fait_dep_saut_multiple config [e;(List.hd q)])
      else false
  
    in if non_pivot  (List.hd list_case) list_case then est_multiple_aux list_case config 
    else false;;

(*fonction dist_but qui prend en parametre une configuration et qui retourne la distance 
du joueur protagoniste au camp d'arrive en faisant la somme (dim- la coordonne i) de chacun 
de ses pions*)
let rec dist_but = fun config -> 
  match config with 
     (list_case_colorees , liste_joueurs ) -> 
      match liste_joueurs with 
      [] -> 0
      |e::q -> 
        match list_case_colorees with 
        [] -> 0
        |x::q -> 
          match x with (case, couleur) -> 
            if couleur = e then match case with 
              (i,j,k) -> if i > dim then 0 + dist_but (q,liste_joueurs)
              else (dim - i)+ dist_but (q,liste_joueurs)
            else dist_but (q,liste_joueurs);; 
(* fonction gagne qui prend une configuration et teste si la distance du joueur protagoniste
au camp d'arrive est nulle (il a gange), faux sinon *)
let gagne = fun config -> 
     if (dist_but config) = 0 then true else false ;;
(* fonction gagnant qui prend en parametre une configuration et verifie si y a un gagnant a l'aide de la 
fonction gagne et retourne sa couleur , Dehors sinon*)
let gagnant = fun config -> match config with 
  (liste_cases,liste_joueurs) -> 
       match liste_joueurs with 
          [] -> Dehors 
          |e::q -> if gagne config then e else 
            Dehors;;

(*fonction est_partie qui prend une configuration et une liste de coup et verifie si chaque coup est valide 
 ( appelle la foction est_dep_unit pour coup unitaire et la fonction est_saut_multiple pour les coup sauts multiple )
et retourne la couleur du gagnant si y en a un , Dehors sinon *)
let rec est_partie = fun config -> fun list_coup -> 
  match list_coup with 
  [] -> Libre
  |e::q -> 
    match e with 
        Du (x,y) -> if est_dep_unit config x y then 
          if dist_but (fait_dep_unit config x y ) = 0 
            then match (fait_dep_unit config x y ) with 
                  (list_cases, liste_joueurs) -> List.hd liste_joueurs
else est_partie (tourne_config (fait_dep_unit config x y)) q
else Dehors
|Sm x -> if est_saut_multiple x config then 
  if dist_but (fait_dep_saut_multiple config x) = 0 
            then match (fait_dep_saut_multiple config x) with 
                  (list_cases, liste_joueurs) -> List.hd liste_joueurs
else est_partie (tourne_config (fait_dep_saut_multiple config x)) q
else Dehors ;;
(*fonction generer_cases_voisines qui prend une case et retourne la liste des cases qui sont voisines a la case prise en parametre 
en ajoutant 1 et -1 a deux coordoonnes et en gardant la troisieme *)
let generer_cases_voisines = fun case -> 
  match case with (i,j,k) -> 
     (i,j+1,k-1)::(i,j-1,k+1)::(i+1,j,k-1)::(i+1,j-1,k)
     ::(i-1,j+1,k)::(i-1,j,k+1)::[];;
(*fonction generer_cases_voisines_accessible qui prend une case et une configuration et verefier
chaque case de la liste des cases voisines (generer par la fonction generer_casese_voisines) est accessible depuis la case 
prise en parametre a l'aide de la fonction est_dep_unit et retourne une liste 
(coup*case) chaque case accessible et le coup pour l'atteindre *)
let generer_cases_voisines_accessible = fun config -> fun case -> 
let rec generer_cases_voisines_accessible_aux = fun config -> fun case -> fun cases_voisines -> 
  match cases_voisines with 
       [] -> [] 
       |e::q -> match e with (i,j,k) -> 
            if est_dep_unit config case e  then 
              (Du(case,e),e)::generer_cases_voisines_accessible_aux config case q
              else 
                generer_cases_voisines_accessible_aux config case q 

              in generer_cases_voisines_accessible_aux config case (generer_cases_voisines case);;
(*fonction generer_cases_ligne_diagonale qui prend en parametre une case et qui retourne 
toutes les cases qui sont allignees avec elle (sur la meme ligne ou la meme diagonale) a l'aide 
des fonctions 
     - generer_cases_droites (retourne la liste des cases situer sur la droite de la case de depart)
     - generer_cases_gauches (retourne la liste des cases situer sur la gauches de la case de depart)
     - generer_cases_diagonale_nord_est(retourne la liste des cases situer a la diagonale nord_est de la case de depart)
      - generer_cases_nord_ouest (retourne la liste des cases situer a la diagonale nord_ouest de la case de depart)
      -generer_cases_sud_est (retourne la liste des cases situer a la diagonale sud_est de la case de depart)
      -generer_cases_sud_ouest(retourne la liste des cases situer a la diagonale sud_ouest de la case de depart) *)
let rec generer_cases_droites = fun case -> fun n -> 
    match case with (i,j,k) -> 
      if n > 2*dim then [] 
      else (i,j+1,k-1)::generer_cases_droites (i,j+1,k-1) (n+1);;
     
let rec generer_cases_gauches = fun case -> fun n -> 
    match case with (i,j,k) -> 
      if n > 2*dim then [] 
      else (i,j-1,k+1)::generer_cases_gauches (i,j-1,k+1) (n+1);;

let rec generer_cases_diagonale_nord_est = fun case -> fun n -> 
    match case with (i,j,k) -> 
      if n > 2*dim then [] 
      else (i+1,j,k-1)::generer_cases_diagonale_nord_est (i+1,j,k-1) (n+1);;


let rec generer_cases_nord_ouest = fun case -> fun n -> 
    match case with (i,j,k) -> 
      if n > 2*dim then [] 
      else (i+1,j-1,k)::generer_cases_nord_ouest (i+1,j-1,k) (n+1);;

let rec generer_cases_sud_ouest = fun case -> fun n -> 
    match case with (i,j,k) -> 
      if n > 2*dim then [] 
      else (i-1,j,k+1)::generer_cases_sud_ouest (i-1,j,k+1) (n+1);;

let rec generer_cases_sud_est = fun case -> fun n -> 
    match case with (i,j,k) -> 
      if n > 2*dim then [] 
      else (i-1,j+1,k)::generer_cases_sud_est (i-1,j+1,k) (n+1);;

let generer_cases_ligne_diagonale = fun case -> 
  (generer_cases_droites case 0)::generer_cases_gauches case 0
  ::generer_cases_diagonale_nord_est case 0
  ::generer_cases_nord_ouest case 0::generer_cases_sud_est 
  case 0::generer_cases_sud_ouest case 0::[];;

(*fonction est_saut1 qui prend en parametre deux cases et une configuration et retourne vrai 
si les deux case sont un saut valide dans la cofiguration en verfiant 
      - la deuxieme case doit etre Libre 
      - si y a un pivot de n'importe quelle couleur entre les deux cases et si les cases entre la premiere case et le pivot 
      et entre la deuxieme case et le pivot sont libres 
      , faux sinon  *)
let est_saut1 = fun c1 -> fun c2 -> fun config -> 
  match config with (list_case_cooloree, list_joueurs) 
    -> match list_joueurs with 
      []-> false 
    |e::q -> if 
  (case_dans_config1 config c2 ) = Libre 
    then 
      match calcul_pivot c1 c2 with 
    None -> false 
    |Some x -> if (est_libre_seg c1 x config)  && (est_libre_seg  x c2 config) && case_dans_config1 config x != Libre
    then true else false 
else false;;
(* fonction est_saut_multiple1 qui prend une liste de cases et une configuration et qui verifie 
si la liste est un saut multiple valide en verifiant 
     - si chaque deux cases consecutifs sont un saut valide dans la config (avce la fonction est_saut1)
     - la derniere case de la liste est bien dans le losange (avec la fonctio est_dans_losange)
     - si la premiere case (case de depart ) n'est pas utiliser comme pivots pour les sauts suivant (fonction non_pivot)*)
let est_saut_multiple1 = fun list_case -> fun config -> 
  let rec est_multiple_aux = fun list_case -> fun config ->  
      match list_case with 
      [] -> false
      |e::[] -> false
      |e::q::[] -> est_saut1 e q config  
      |e::q -> if est_saut1 e (List.hd q) config && 
        est_dans_losange (List.nth list_case ((List.length list_case)-1)) then 
        est_multiple_aux q (fait_dep_saut_multiple config [e;(List.hd q)])
      else false
  
    in if non_pivot  (List.hd list_case) list_case then est_multiple_aux list_case config 
    else false;;


(* fonction generer_cases_saut_accessible qui prend en parametre une config et une case et verifie
la liste des cases generer par la fonction generer_cases_ligne_diagonale si elles sont accessible depuis la case de depart a l'aide de 
la fonction est_saut1 et retourne les cases accessible en un coup et les coups pour les atteindre *)
let generer_cases_saut_accessible = fun config -> fun case -> 
  let rec generer_cases_saut_accessible_aux = fun config 
  -> fun case -> fun liste_cases_ligne_diagonale -> 
    match liste_cases_ligne_diagonale with 
       [] -> []
       |e::q -> match e with 
           [] -> generer_cases_saut_accessible_aux config case q 
           |x::r -> if est_saut1 case x config
            then (Sm [case;x],x)::
           generer_cases_saut_accessible_aux config case (r::q)
else  generer_cases_saut_accessible_aux config case (r::q) in 
generer_cases_saut_accessible_aux config case (generer_cases_ligne_diagonale case);;

(*fonction concatener_deux_listes qui prend deux listes et les fusionne*)
let rec concatener_deux_listes = fun l1 -> fun l2 -> 
  match l1 with 
       [] -> l2 
       |e::q -> concatener_deux_listes q (e::l2) ;;

           


(*fonction est_dep_unit1 qui prend une configuration et deux cases et qui verfie si le déplacement des deux cases est unitaire
en vérifiant les couleurs des cases avec la fonction case_dans_config1 et si les deux cases sont voisines avec la fonction 
sont_voisines et si la case vers laquelle on veut se déplacer est bien dans le losange nord-sud 
si toutes les conditions sont satisfaites elle renvoie vrai, faux sinon*)
let rec est_dep_unit1 = fun configuration -> fun case_1 -> fun case_2 -> 
  match configuration with (list_case_cooloree, list_joueurs) 
    -> match list_joueurs with 
      []-> false 
    |e::q -> if (case_dans_config1 configuration case_2)= Libre
                &&  (est_dans_losange case_2) && (sont_cases_voisines case_1 case_2)  then  
        true else false ;;

(*fonction coup_possible qui prend une configuration et une case et retourne la liste des cases accessibles 
et les coups pour les atteindre en fusionnant la liste des cases generer par la fonction generer_cases_voisines_accessible et 
la liste des cases generer par la fonction generer_cases_saut_accessible *)
let coup_possible = fun config -> fun case -> 
  let rec coup_possible_aux  = fun config -> fun liste -> match liste 
          with 
        
        [] -> []
          |e::q -> match e with 
              (Du (x,y), z) ->  if est_dep_unit1 config x y then (Du (x,y), z)::coup_possible_aux config q
              else coup_possible_aux config q
            |Sm l,z -> if est_saut_multiple1 l config then 
                 (Sm l,z)::coup_possible_aux config q else 
                coup_possible_aux config q 
                 in coup_possible_aux config 
         (concatener_deux_listes
         (generer_cases_voisines_accessible config case)
          (generer_cases_saut_accessible config case)) ;;

(*fonction meilleur_coup_compare qui prend une config et deux coups et retourne le meilleur des 
deux c’est à dire celui qui fait décroître le plus la distance au but *)
let meilleur_coup_compare = fun config -> fun coup1 -> fun coup2 -> 
      match coup1 with 
         Du (x,y) ->(match coup2 with 
         Du (a,b) -> if (dist_but (fait_dep_unit config x y)) < dist_but (fait_dep_unit config a b)
             then Du (x,y) else Du (a,b) 
        |Sm q -> if (dist_but (fait_dep_saut_multiple config q) < (dist_but (fait_dep_unit config x y)))
          then Sm q else Du (x,y))
        |Sm r -> match coup2 with 
        Du (a,b) -> if (dist_but (fait_dep_saut_multiple config r)) < dist_but (fait_dep_unit config a b)
        then Sm r else Du (a,b) 
        |Sm q -> if (dist_but (fait_dep_saut_multiple config q) < (dist_but (fait_dep_saut_multiple config r)))
          then Sm q else Sm r;;

(* fonction coups_possible_list qui prend une liste (coup*case) et renvoie
juste les coups contenue dans cette liste *)
let rec coups_possible_list = fun liste -> 
  match liste with 
    [] -> []
    |e::q -> 
      match e with 
        (Du (x,y),z) -> (Du (x,y))::coups_possible_list q 
        |(Sm r,z) -> (Sm r)::coups_possible_list q;;
(* fonction meilleur_coup_case qui prend une config et une case et renvoie le meilleur coup pour 
cette case c’est à dire celui qui fait décroître le plus la distance au but parmis la liste
des coups possible generer par la fonction coup_possible *)
let meilleur_coup_case = fun config -> fun case -> 
      let rec meilleur_coup_case_aux = fun config -> fun case -> 
          fun liste -> match liste with 
            [] -> Sm []
             |e::[] -> e 
             |e1::e2::[] -> meilleur_coup_compare config e1 e2 
             |e::q -> meilleur_coup_case_aux config case (meilleur_coup_compare 
             config e (List.hd q)::q)
      in meilleur_coup_case_aux config case 
      (coups_possible_list (coup_possible config case));;

(*fonction meilleurs_coup_chaque_case qui prend en parametre une config et retourne la liste 
des meilleurs coups pour chaque case a l'aide de la fonction meilleur_coup_case qui genere le meuilleur 
coup possible pour une case *)
let rec meilleurs_coups_chaque_case = fun config -> 
     match config with 
        (liste_cases_coolore, liste_joueur) -> 
           match liste_joueur with 
              [] -> []
              |e::q -> match liste_cases_coolore with 
                  [] -> []
                  |z::r -> match z with (x,t) -> if (case_dans_config1 config x )= e then 
                         (meilleur_coup_case config x)::meilleurs_coups_chaque_case (r,liste_joueur)
                else  meilleurs_coups_chaque_case (r,liste_joueur) ;;

(*fonction meilleurs_coup qui prenden parametre uen configuration et retourne le meilleur 
coup possible pour le joueur protagoniste en comparant les coups de la liste des meilleurs coups de chaque case
(generer par la fonction meilleurs_coup_chaque_case) a l'aide de la fonction meilleur_coup_compare*)
let meilleurs_coup = fun config -> 
   let rec meilleurs_coup_aux = fun config -> fun liste ->  
     match liste with 
        [] -> Sm []
        |e::[] -> e
         |e1::e2::q -> if (meilleur_coup_compare config e1 e2) = e1 then 
             meilleurs_coup_aux config (e1::q) else 
              meilleurs_coup_aux config (e2::q)
             in meilleurs_coup_aux config (meilleurs_coups_chaque_case config);;



(*fonction mis_a_jour_configuration qui prend une configuration et le coup a jouer 
 et retourne soit une erreur si 
    - le coup n'est pas valide (a l'aide de la fonction est_dep_unit ou est_saut_multiple) 
    - le coup a fait gagner le joueur protagoniste 
    
    ou bien retourne une nouvelle configuration avec le coup effectue (a l'aide des fonctions 
    tourne_config, fait_dep_unit,fait_dep_saut_multiple )
    *)
let mis_a_jour_configuration  = fun conf -> fun coup -> 
  match coup with 
    Du (c1,c2) -> if (est_dep_unit conf c1 c2) = true && (gagnant (fait_dep_unit conf c1 c2)) != Dehors then
      Error (" fin de partie ! le joueur "^string_of_couleur (gagnant (fait_dep_unit conf c1 c2))^" a gagné !") else if (est_dep_unit conf c1 c2) = true 
    then  Ok (tourne_config(fait_dep_unit conf c1 c2))
    else Error ("ce n'est pas un coup valide !")

  |  Sm (coup) -> match coup with 
      []-> Error ("ce n'est pas un coup valide !")
    |_ -> if (est_saut_multiple coup conf) = true && (gagnant (fait_dep_saut_multiple conf coup)) != Dehors 
      then Error (" fin de partie ! le joueur "^string_of_couleur (gagnant (fait_dep_saut_multiple conf coup))^" a gagné !") 
         else if (est_saut_multiple coup conf) = true && 
        (gagnant (fait_dep_saut_multiple conf coup)) = Dehors then Ok (tourne_config(fait_dep_saut_multiple conf coup))
    else Error ("ce n'est un coup valide !");;
