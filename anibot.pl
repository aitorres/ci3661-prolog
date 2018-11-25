/** AniBot

Este módulo incluye la implementación de un bot de Prolog
escrito en el dialecto SWI-Prolog que reconoce lenguaje natural
y permite establecer una conversación sobre series de animé,
géneros, ratings y popularidad.

@author Gustavo Castellanos (14-10192)
@author Andrés Ignacio Torres (14-11082)
@license MIT
*/

:- use_module(library(random)).

% ==========================================================================
% Predicados sobre animé
% ==========================================================================

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

% ==========================================================================
% Mensajes del bot
% ==========================================================================

/**
 * chat/2
 * 
 * es_mensaje(X, Y) acierta si Y es una lista de mensajes de tipo X.
 *
 * Uso auxiliar pasando un parámetro tipo, para obtener una lista de
 * mensajes de dicho tipo.
 */
es_mensaje(
    "bienvenida", 
    [
        "Yuki:- Hola, soy Yuki. ¿Quieres hablar?",
        "Yuki:- *se asoma, tímidamente* Hola...",
        "Yuki:- ¡HOLA! HABLEMOS.",
        "Yuki:- ¿H-hola? ¿Está-á-ás a-a-ahí?",
        "Yuki:- Un gusto, humano. Soy una interfaz automatizada para conocer series de animé."
    ]
).

es_mensaje(
	"despedida",
	[
		"Yuki:- Hasta luego, humano.",
		"Yuki:- Ya he aprendido suficiente de ti. Puedes irte.",
		"Yuki:- C-creo que m-m-me voy...",
		"Yuki:- Oh, humano, me llaman en otra interfaz. Debo irme.",
		"Yuki:- Hasta luego, humano. Gracias por todo.",
		"Yuki:- Debería irme. Dejé la tetera encendida.",
		"Yuki:- Adieu!~"
	]
).

es_mensaje(
	"clima",
	[
		"Yuki:- El clima es una construcción social. No entiendo de eso.",
		"Yuki:- Hablando del clima, mi nombre significa 'nieve'. Kawaii!~",
		"Yuki:- Gomenasai, no comprendo el concepto humano de clima.",
		"Yuki:- Ah, sí, el clima está muy bonito acá, muchos bytes y pocos bits.",
		"Yuki:- Está un poco caluroso acá, ¿no crees?",
		"Yuki:- No entiendo de clima, pero hay otro bot amigo mío que sí. Creo que se llama @USBClima en Twitter.",
		"Yuki:- ¿Por qué me habla sobre el clima si sabes que no sé de eso?",
		"Yuki:- El 'clima' no es un tema de animé. A veces, pienso que no eres muy inteligente...",
		"Yuki:- ¿Clima? ¿Qué es eso?"
	]
).

% ==========================================================================
% Funciones auxiliares del bot
% ==========================================================================

/**
 * acceder/3
 * 
 * acceder(L, I, M) acierta si M es el elemento con índice I (indexado en 1)
 * en la lista L.
 *
 * Función auxiliar para utilizar listas como arreglos usuales.
 */
acceder([X | _], 1, X).
acceder([_| Xs], Indice, M):-
    Nuevo_Indice is Indice-1,
    acceder(Xs, Nuevo_Indice, M).

/**
 * obtener_mensaje_aleatorio/2
 * 
 * obtener_mensaje_aleatorio(L, M) unifica en M un mensaje obtenido
 * aleatoriamene entre los existentes en la lista L. Más generalmente,
 * unifica en M un elemento obtenido aleatoriamente en la lista L.
 *
 * Función auxiliar para obtener mensajes de respuesta de manera dinámica.
 */
obtener_mensaje_aleatorio(Tipo, Mensaje):-
    es_mensaje(Tipo, Lista),
    length(Lista, Tamano_lista),
    Tope is Tamano_lista + 1,
    random(1, Tope, Indice),
    acceder(Lista, Indice, Mensaje).

% ==========================================================================
% Funciones auxiliares de I/O del bot
% ==========================================================================

/**
 * leer/1
 *
 * leer(M) realiza la operación de I/O de lectura de la entrada estándar
 * para obtener un string por el usuario hasta encontrar un salto
 * de línea, que es unificado con el parámetro M
 */
leer(M):-
    read_string(user_input, "\n", "\r", _, M).

/**
 * imprimir/1
 *
 * imprimir(M) imprime un mensaje en pantalla; seguidamente,
 * imprime un salto de línea y hace flush_output para asegurar que 
 * el orden de operaciones de I/O sea el esperado
 */
imprimir(M):-
    write_term(
        M, 
        [ 
            portray(true),
            numbervars(true),
            quoted(false)
        ]
    ),
    nl,
    flush_output.

/**
 * imprimir_prompt/0
 *
 * imprimir_prompt muestra en pantalla una decoración de tipo
 * prompt para el usuario, ideal para ser utilizado justo antes de 
 * solicitar input, ya que no imprime un salto de línea
 */
imprimir_prompt:-
    write_term(
        "Yo:- ", 
        [ 
            portray(false),
            numbervars(true),
            quoted(false)
        ]
    ),
    flush_output.

/**
 * separar_frase/2
 *
 * separar_frase(F, L) separa una string F por espacios o puntos y unifica
 * cada palabra hallada en la lista L
 */
separar_frase(F, L):-
	split_string(F, " ", " .,", L).

/**
 * es_palabra_de/2
 *
 * es_palabra_de(S, F) acierta si S es una palabra de la frase F, separada
 * por espacios; si no se pasa S, se unifica con alguna de las posibles
 * palabras de F.
 */
es_palabra_de(S, F):-
	separar_frase(F, L),
	member(S, L).

% ==========================================================================
% Parseo de temas de entrada
% ==========================================================================

/**
 * es_despedida/1
 *
 * es_despedida(M) acierta si la string M contiene alguna palabra
 * clave que identifice una despedida.
 */
es_despedida(M):-
	(es_palabra_de("adios", M); es_palabra_de("Adios", M);
	es_palabra_de("adiós", M); es_palabra_de("Adiós", M);
	es_palabra_de("chao", M); es_palabra_de("Chao", M);
	es_palabra_de("hasta luego", M); es_palabra_de("Hasta luego", M);
	es_palabra_de("quit", M); es_palabra_de("Quit", M)).
  
/**
 * es_clima/1
 *
 * es_clima(M) acierta si la string M contiene alguna palabra
 * clave que identifique que habla sobre el clima
 */
es_clima(M):-
	(es_palabra_de("clima", M); es_palabra_de("Clima", M)).

% ==========================================================================
% Funciones auxiliares de conversación del bot
% ==========================================================================

/**
 * dar_bienvenida/0
 * 
 * dar_bienvenida muestra en pantalla un mensaje de bienvenida
 * escogido aleatoriamente entre los mensajes existentes.
 */
dar_bienvenida:-
    obtener_mensaje_aleatorio("bienvenida", M),
    imprimir(M).

/**
 * reponder/1
 *
 * responder(M) determina si M es un mensaje apropiado de salida
 * del usuario, en cuyo caso imprime un mensaje de despedida
 * y finaliza la ejecución; en caso contrario, falla (para saltar
 * al próximo predicado)
 */
responder(M):-
    es_despedida(M), !,
    obtener_mensaje_aleatorio("despedida", D),
    imprimir(D),
    halt.

/**
 * reponder/1
 *
 * responder(M) determina si M es un mensaje sobre el clima
 * del usuario, en cuyo caso imprime un mensaje al respecto
 * y falla.
 */
responder(M):-
    es_clima(M), !,
    obtener_mensaje_aleatorio("clima", D),
    imprimir(D),
    fail.

/**
 * responder/1
 *
 * responder(M) interpreta el mensaje M del usuario y genera una
 * respuesta apropiada.
 *
 * @todo: terminar la implementación de este predicado
 */
responder(M):-
    % Este predicado debería interpretar M
    % y generar una respuesta acorde, pero...
    string_concat("Yuki:- Acabas de decir: ", M, Mf), % debug
    imprimir(Mf), % debug
    imprimir("Yuki:- No estoy capacitada para responderte aún."), %debug
    fail. % esto NO es debug, esta regla siempre debe fallar para backtrackear


% ==========================================================================
% Funciones principales del chat
% ==========================================================================

/**
 * conversar/0
 *
 * conversar repite en un ciclo infinito (por backtracking) una lógica sencilla
 * de conversación bilateral: se lee un mensaje del usuario y se responde por 
 * parte del bot.
 */
conversar:-
    repeat,
    imprimir_prompt, % mostramos un prompt decorativo
    leer(M), % leemos la entrada del usuario
    responder(M). % ejecutamos una acción de acuerdo a lo solicitado
    % NOTA: reponder/1 termina la ejecución, o falla y asegura el backtracking

/**
 * chat/0
 * 
 * chat muestra un mensaje de bienvenida en pantalla e inicia
 * el ciclo de la conversación con el bot
 */
chat:-
    dar_bienvenida,
    conversar.
