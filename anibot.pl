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
        "Digimon"
    ]
).
    
/**
 * genero(X:string) es determinado
 *
 * Genero acierta si X es un género de animé
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
    
    generoAnime("Naruto",["Shounen","Aventura"]).
    generoAnime("Dragon Ball",["Shounen"]).
    generoAnime("Bleach",["Shounen", "Sobrenatural"]).
    generoAnime("HunterXHunter",["Seinen", "Aventura"]).
    generoAnime("Hamtaro",["Kodomo"]).
    generoAnime("Full Metal Alchemist",["Shounen", "Magia"]).
    
    rating("Dragon Ball",3).
    rating("Naruto",1).
    rating("Bleach",4).
    rating("HunterXHunter",5).
    rating("Hamtaro",2).
    rating("Full Metal Alchemist",4).
    
    popularidad("Dragon Ball",7).
    popularidad("Naruto",5).
    popularidad("Bleach",8).
    popularidad("HunterXHunter",3).
    popularidad("Hamtaro",10).
    popularidad("Full Metal Alchemist",1).