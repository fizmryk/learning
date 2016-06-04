module Slownie (Rodzaj(..), Waluta(..), slownie) where
	import Data.List
	data Rodzaj = Meski | Zenski | Nijaki deriving Show
	
	data Waluta = Waluta
		{
		mianownik_poj :: String,
		mianownik_mn :: String,
		dopelniacz_mn :: String,
		rodzaj :: Rodzaj
		} deriving Show
		
		
	
	slownie :: Waluta -> Integer -> String --główna funkcja
	slownie waluta liczba =
		if (liczba == 0) then "zero " ++ (dopelniacz_mn waluta) --wariant, gdy liczba = 0
		else (if (liczba < 0) then "minus " --wariant, gdy liczba jest ujemna
		else []) ++ --konkatenacja
		(if (abs liczba == 1) then (translate_one $ rodzaj waluta) ++ (translate_currency_one waluta) --wariant, gdy |liczba| = 1
		else (sklej_liste xs) ++ (translate_last el waluta) ) --wariant, gdy |liczba| > 1 - ostatni element tablicy tłumaczony osobno
		where
		tab = calculate (abs liczba)
		el = last tab
		xs = sklej_pary ( translate_list (delete el tab) )
	
	
	
	sklej_liste :: [String] -> String --funkcja sklejająca napisy w liście typu: ["jedenaście milionów", "dwadzieścia cztery tysiące"]
	sklej_liste = foldl (++) []
	
	sklej_pary :: [(String,String)] -> [String] --funkcja sklejająca pary napisów w liście typu: [("jedenaście", "milionów"), ("sto", "tysięcy")]
	sklej_pary = map (sklej_pare)
	
	sklej_pare :: (String, String) -> String --funkcja sklejająca parę napisów w jeden napis
	sklej_pare (s1,s2) = s1 ++ s2
	
	
	
	calculate :: Integer -> [(Integer, Integer)] --rozkładanie liczby do zapisu - funkcja tworząca listę krotek [(n1,k1),(n2,k2),...(nk,kk)], gdzie n1,n2,...,nk <= 999, a k1,k2,...,kk to kolejne potęgi tysiąca ułożone malejąco 
	calculate n = reverse(aux n 0) where
	aux 0 _ = []
	aux n k = ( (moded, k) ):( aux divided (k+1) )
		where
		divided = n `div` 1000
		moded   = n `mod` 1000 
		
	
	translate_list :: [(Integer, Integer)] -> [(String, String)] -- funkcja tworząca pary stringów do sklejenia
	translate_list xs =  map (translate_pair) (xs)  

	

	translate_pair :: (Integer, Integer) -> (String, String) --funkcja biorącą parę liczb (n,k) i zamieniającą ją na słowną reprezentację (string_n, string_k)
	translate_pair (n,k) = (translate_n n, translate_k n k)
	
	translate_last :: (Integer, Integer) -> Waluta -> String --funkcja tłumacząca ostatnią parę (n,k), gdzie k = 0 (razem z odmianą rzeczownika podanej waluty)
	translate_last (n,0) waluta =
		(if (n >= 100) then
			if (reszta >= 20) then
				(translate_hundreds hundreds) ++ (translate_tens reszta) ++ (translate_gender (digits) (rodzaj waluta)) ++ (translate_currency reszta waluta)
			else if (reszta >= 10) then
				(translate_hundreds hundreds) ++ (translate_teen reszta) ++ (translate_currency reszta waluta)
			else (translate_hundreds hundreds) ++ (translate_gender (digits) (rodzaj waluta)) ++ (translate_currency reszta waluta)
		else if (n >= 20) then (translate_tens reszta) ++ (translate_gender (digits) (rodzaj waluta) ) ++ (translate_currency reszta waluta)
		else if (n >= 10) then (translate_teen reszta) ++ (translate_currency reszta waluta)
		else (translate_gender reszta (rodzaj waluta) ) ++ (translate_currency reszta waluta) )
		where
			hundreds = n `div` 100
			reszta = n `mod` 100
			digits = n `mod` 10

	
	translate_gender :: Integer -> Rodzaj -> String --przypadek, gdy odmieniamy rząd cyfr, gdy liczba >=20
	translate_gender digit rodzaj =
		if (digit == 0) then ""
		else if (digit == 1) then "jeden "
		else if (digit == 2) then
			if (show rodzaj == "Zenski") then translate_woman_two digit
			else translate_other_two digit
		else if (digit == 3) then "trzy "
		else if (digit == 4) then "cztery "
		else if (digit == 5) then "pięć "
		else if (digit == 6) then "sześć "
		else if (digit == 7) then "siedem "
		else if (digit == 8) then "osiem "
		else "dziewięć "
	
	translate_one :: Rodzaj -> String --przypadek, gdy odmieniamy liczbę 1
	translate_one rodzaj = if (show rodzaj == "Zenski") then translate_woman_one
		else if (show rodzaj == "Meski") then translate_man_one
		else translate_this_one
	
	translate_woman_one :: String
	translate_woman_one = "jedna "
	
	translate_this_one :: String
	translate_this_one = "jedno "
	
	translate_man_one :: String
	translate_man_one = "jeden "
	
	translate_woman_two :: Integer -> String
	translate_woman_two digit = "dwie "
		
	translate_other_two :: Integer -> String
	translate_other_two digit = "dwa "
	
	translate_currency :: Integer -> Waluta -> String --odmienianie rzeczownika
	translate_currency reszta waluta =
		if (reszta >= 20) then translate_currency_not_teens (reszta `mod` 10) waluta
		else if (reszta >= 10) then translate_currency_teens waluta
		else translate_currency_not_teens reszta waluta
	
	translate_currency_one :: Waluta -> String
	translate_currency_one waluta = mianownik_poj waluta
	
	translate_currency_teens :: Waluta -> String
	translate_currency_teens waluta = dopelniacz_mn waluta

	translate_currency_not_teens :: Integer -> Waluta -> String
	translate_currency_not_teens digit waluta =
		if (digit >= 2 && digit <= 4) then mianownik_mn waluta
		else dopelniacz_mn waluta
		
	
	--TŁUMACZENIE LICZB 0-999
	translate_n :: Integer -> String
	translate_n n = 
		( 
		if (n == 0 || n == 1) then ""
		else if (n >= 100) then	
			if (tens) >= 10 && (tens) <= 19 then (translate_hundreds hundreds ) ++ (translate_teen tens )
			else if (tens) > 19 then (translate_hundreds hundreds) ++ (translate_tens tens) ++ (translate_digits digits)
			else (translate_hundreds hundreds) ++ (translate_digits digits)
	
		else if n >= 10 then
			if (tens) >= 10 && (tens) <= 19 then (translate_teen tens)
			else (translate_tens tens) ++ (translate_digits digits)
	
		else translate_digits n
		)
		where
			hundreds = n `div` 100
			tens = n `mod` 100
			digits = n `mod` 10
	
	
	translate_hundreds :: Integer -> String
	translate_hundreds n = if (n == 1) then "sto "
	else if (n == 2) then "dwieście "
	else if (n == 3) then "trzysta "
	else if (n == 4) then "czterysta "
	else if (n == 5) then "pięćset "
	else if (n == 6) then "sześćset "
	else if (n == 7) then "siedemset "
	else if (n == 8) then "osiemset "
	else "dziewięćset "
	
	translate_tens :: Integer -> String
	translate_tens n = if (n `div` 10 == 2) then "dwadzieścia "
	else if (n `div` 10 == 3) then "trzydzieści " 
	else if (n `div` 10 == 4) then "czterdzieści " 
	else if (n `div` 10 == 5) then "pięćdziesiąt " 
	else if (n `div` 10 == 6) then "sześćdziesiąt " 
	else if (n `div` 10 == 7) then "siedemdziesiąt " 
	else if (n `div` 10 == 8) then "osiemdziesiąt "
	else "dziewięćdziesiąt "
	
	translate_teen :: Integer -> String
	translate_teen n = if (n `mod` 10 == 0) then "dziesięć "
	else if (n `mod` 10 == 1) then "jedenaście "
	else if (n `mod` 10 == 2) then "dwanaście "
	else if (n `mod` 10 == 3) then "trzynaście "
	else if (n `mod` 10 == 4) then "czternaście "
	else if (n `mod` 10 == 5) then "piętnaście "
	else if (n `mod` 10 == 6) then "szesnaście "
	else if (n `mod` 10 == 7) then "siedemnaście "
	else if (n `mod` 10 == 8) then "osiemnaście "
	else "dziewiętnaście "
	
	translate_digits :: Integer -> String
	translate_digits n = if (n == 1) then "jeden "
	else if (n == 2) then "dwa "
	else if (n == 3) then "trzy "
	else if (n == 4) then "cztery "
	else if (n == 5) then "pięć "
	else if (n == 6) then "sześć "
	else if (n == 7) then "siedem "
	else if (n == 8) then "osiem "
	else if (n == 9) then "dziewięć "
	else ""
	
	translate_k :: Integer -> Integer -> String
	translate_k n k = if (k == 1) then translate_thousands n 
	else (translate_power n k) ++ (add_suffix n)
	
	
	-- TRANSLACJA POTĘGI 1000^1, czyli dla tysiąca
	translate_thousands :: Integer -> String
	translate_thousands n = 
		(if (n >= 100) then
			if (reszta) == 0 then "tysięcy "
			else if (reszta >= 10 && reszta <= 19) then "tysięcy "
			else case (reszta `mod` 10) of
				0 -> "tysięcy "
				1 -> "tysięcy "
				2 -> "tysiące "
				3 -> "tysiące "
				4 -> "tysiące "
				5 -> "tysięcy "
				6 -> "tysięcy "
				7 -> "tysięcy "
				8 -> "tysięcy "
				9 -> "tysięcy " 
		else if (n >= 20) then
			case (reszta `mod` 10) of
				0 -> "tysięcy "
				1 -> "tysięcy "
				2 -> "tysiące "
				3 -> "tysiące "
				4 -> "tysiące "
				5 -> "tysięcy "
				6 -> "tysięcy "
				7 -> "tysięcy "
				8 -> "tysięcy "
				9 -> "tysięcy "
		else if (n >= 10 && n <= 19) then "tysięcy "
		else case (reszta `mod` 10) of
				0 -> ""
				1 -> "tysiąc "
				2 -> "tysiące "
				3 -> "tysiące "
				4 -> "tysiące "
				5 -> "tysięcy "
				6 -> "tysięcy "
				7 -> "tysięcy "
				8 -> "tysięcy "
				9 -> "tysięcy " )
		where
		reszta = n `mod` 100
	
	
	--DODAWANIE PRZYROSKTÓW DO POTĘG WYŻSZYCH, NIŻ 1000^1
	add_suffix :: Integer -> String
	add_suffix n = ( if (n == 0) then "" 
	else if (n >= 100) then
		if (reszta >= 20) then
			case (reszta `mod` 10) of
				0 -> "ów "
				1 -> "ów "
				2 -> "y "
				3 -> "y "
				4 -> "y "
				5 -> "ów "
				6 -> "ów "
				7 -> "ów "
				8 -> "ów "
				9 -> "ów "
		else if (reszta == 0) then "ów "
		else if (reszta >= 10 && reszta <= 19) then "ów "
		else case reszta of
			0 -> "ów "
			1 -> " "
			2 -> "y "
			3 -> "y "
			4 -> "y "
			5 -> "ów "
			6 -> "ów "
			7 -> "ów "
			8 -> "ów "
			9 -> "ów "
	else if (n >= 20) then
		case (reszta `mod` 10) of
			0 -> "ów "
			1 -> " "
			2 -> "y "
			3 -> "y "
			4 -> "y "
			5 -> "ów "
			6 -> "ów "
			7 -> "ów "
			8 -> "ów "
			9 -> "ów "
	
	else if (n < 10) then
		case reszta of
			0 -> " "
			1 -> " "
			2 -> "y "
			3 -> "y "
			4 -> "y "
			5 -> "ów "
			6 -> "ów "
			7 -> "ów "
			8 -> "ów "
			9 -> "ów "
	else "ów " )
		where
		reszta = n `mod` 100
	
	--TRANSLACJA POTĘG 1000^k, gdzie k > 1
	translate_power :: Integer -> Integer -> String
	translate_power n k = ( if n == 0 then ""
	else if (real_k < 10) then translate_low_power real_k i 
	else translate_high_power real_k i)
		where
		real_k = k `div` 2
		i = k `mod` 2
	
	--TRANSLACJA POTĘG 1000^(2k+i), gdzie k > 1 oraz k < 10 oraz (i = 1 lub i = 0)
	translate_low_power :: Integer -> Integer -> String
	translate_low_power k i = (case k of 
			1 -> "mil"
			2 -> "bil"
			3 -> "tryl"
			4 -> "kwadryl"
			5 -> "kwintyl"
			6 -> "sekstyl"
			7 -> "septyl"
			8 -> "oktyl"
			9 -> "nonil" ) ++ 
			  (case i of 
					0 -> "ion"
					1 -> "iard")
	translate_high_power :: Integer -> Integer -> String
	translate_high_power real_k i = ( (case a of
			  1 -> "un"
			  2 -> "do"
			  3 -> "tri"
			  4 -> "kwatuor"
			  5 -> "kwint"
			  6 -> "seks"
			  7 -> "septen"
			  8 -> "okto"
			  9 -> "nowem"
			  0 -> "") ++
			 (case b of
			   1 -> "decy"
			   2 -> "wicy"
			   3 -> "trycy"
			   4 -> "kwadragi"
			   5 -> "kwintagi"
			   6 -> "seksginty"
			   7 -> "septagi"
			   8 -> "oktagi"
			   9 -> "nonagi"
			   0 -> "") ++
		     (case c of 
			   1 -> "centy"
			   2 -> "ducenty"
			   3 -> "trycenty"
			   4 -> "kwadryge"
			   5 -> "kwinge"
			   6 -> "sescenty"
			   7 -> "septynge"
			   8 -> "oktynge"
			   9 -> "nonge"
			   0 -> "") ++
			  (case i of
			   1 -> "liard"
			   0 -> "lion") )
		where 
		a = real_k `mod` 10
		b = (real_k `div` 10) `mod` 10
		c = real_k `div` 100