/** AniBot

Este módulo incluye la implementación de un bot de Prolog
escrito en el dialecto SWI-Prolog que reconoce lenguaje natural
y permite establecer una conversación sobre series de animé,
géneros, ratings y popularidad.

@author Gustavo Castellanos (14-10192)
@author Andrés Ignacio Torres (14-11082)
@license MIT
*/

% ============================================================
% Predicados sobre animé
% ============================================================

/**
 * anime(X:string) es determinado
 *
 * anime/1 acierta si X es un animé
 */
anime(X) :- member(
    X, 
    [
        "Dragon Ball",
        "Naruto",
        "Bleach",
        "HunterXHunter",
        "Hamtaro",
        "Full Metal Alchemist",
        "Suzumiya Haruhi no Yuutsu",
        "Sword Art Online",
        "Another",
        "Death Note",
        "Attack on Titan",
        "Steins;Gate",
        "Pokémon",
        "InuYasha",
        "Kuroshitsuji",
        "Yu-Gi-Oh!",
        "Digimon",
        "Eureka Seven"
    ]
).
    
/**
 * genero(X:string) es determinado
 *
 * genero/1 acierta si X es un género de animé
 */
genero(X) :- member(
    X,
    [
        "Aventura",
        "Shoujo",
        "Shounen",
        "Kodomo",
        "Seinen",
        "Josei",
        "Ficción",
        "Fantasía",
        "Mecha",
        "Sobrenatural",
        "Magia",
        "Gore"
    ]
).

/**
 * generoAnime(X:string, Y:[string])
 *
 * generoAnime/2 acierta si Y es la lista de géneros de un anime;
 *               si no se pasa, Y se unifica con la lista de géneros del animé X;
 *               si no se pasa, X se unifica con algún animé cuya 
 *               lista de géneros sea Y.
 */
generoAnime("Naruto", ["Shounen","Aventura"]).
generoAnime("Dragon Ball", ["Shounen"]).
generoAnime("Bleach", ["Shounen", "Sobrenatural"]).
generoAnime("HunterXHunter", ["Seinen", "Aventura"]).
generoAnime("Hamtaro", ["Kodomo"]).
generoAnime("Full Metal Alchemist", ["Shounen", "Magia"]).
generoAnime("Suzumiya Haruhi no Yuutsu", ["Aventura", "Fantasía", "Sobrenatural"]).
generoAnime("Sword Art Online", ["Aventura", "Ficción", "Fantasía"]).
generoAnime("Another", ["Aventura", "Sobrenatural", "Gore"]).
generoAnime("Death Note", ["Aventura", "Sobrenatural", "Ficción"]).
generoAnime("Attack on Titan", ["Aventura", "Gore"]).
generoAnime("Steins;Gate", ["Ficción", "Aventura", "Sobrenatural"]).
generoAnime("Pokémon", ["Fantasía", "Aventura", "Kodomo"]).
generoAnime("InuYasha", ["Aventura", "Shoujo"]).
generoAnime("Kuroshitsuji", ["Shoujo"]).
generoAnime("Yu-Gi-Oh!", ["Magia", "Ficción", "Shounen"]).
generoAnime("Digimon", ["Fantasía", "Aventura", "Shounen"]).
generoAnime("Eureka Seven", ["Aventura", "Mecha", "Magia"]).

/**
 * rating(X:string, Y:int)
 *
 * rating/2 acierta si Y es la puntuación entre 1 y 5 del animé X;
 *          si no se pasa, Y se unifica con la puntuación del animé X;
 *          si no se pasa, X se unifica con algún animé cuya 
 *          puntuación sea Y.
 */
rating("Dragon Ball", 3).
rating("Naruto", 1).
rating("Bleach", 4).
rating("HunterXHunter", 5).
rating("Hamtaro", 1).
rating("Full Metal Alchemist", 4).
rating("Suzumiya Haruhi no Yuutsu", 3).
rating("Sword Art Online", 4).
rating("Another", 4).
rating("Death Note", 5).
rating("Attack on Titan", 5).
rating("Steins;Gate", 2).
rating("Pokémon", 4).
rating("InuYasha", 4).
rating("Kuroshitsuji", 2).
rating("Yu-Gi-Oh!", 3).
rating("Digimon", 4).
rating("Eureka Seven", 3).

/**
 * popularidad(X:string, Y:int)
 *
 * popularidad/2 acierta si Y es la popularidad entre 1 y 10 del animé X;
*                si no se pasa, Y se unifica con la popularidad del animé X;
*                si no se pasa, X se unifica con algún animé cuya 
*                popularidad sea Y.
 */
popularidad("Dragon Ball", 7).
popularidad("Naruto", 5).
popularidad("Bleach", 8).
popularidad("HunterXHunter", 3).
popularidad("Hamtaro", 10).
popularidad("Full Metal Alchemist", 1).
popularidad("Suzumiya Haruhi no Yuutsu", 6).
popularidad("Sword Art Online", 9).
popularidad("Another", 5).
popularidad("Death Note", 10).
popularidad("Attack on Titan", 10).
popularidad("Steins;Gate", 4).
popularidad("Pokémon", 10).
popularidad("InuYasha", 8).
popularidad("Kuroshitsuji", 3).
popularidad("Yu-Gi-Oh!", 7).
popularidad("Digimon", 8).
popularidad("Eureka Seven", 2).