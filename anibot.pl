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

% Comentario de los autores: Prolog debería tener este predicado nativo :-(
and(A, B):- A, B.
and(A, B, C):- A, B, C.
and(A, B, C, D):- A, B, C, D.

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

/**
 * ratingPopularidad/2
 * 
 * ratingPopularidad(A, X) acierta si el animé A 
 * tiene un valor sumado (previamente calculado) de rating
 * y puntuación X
 */
ratingPopularidad(A, X):- 
	anime(A),
	popularidad(A, P),
	rating(A, R),
	X is P+R.

% ==========================================================================
% Funciones auxiliares de animé
% ==========================================================================

/**
 * anime_segun_rating/2
 *
 * Si R es un entero entre 1 y 5, unifica en L una lista con todos los
 * animé cuyo rating es R.
 */
anime_segun_rating(R, L):-
	R >= 1,
	5 >= R,
	findall(X, rating(X, R), L).

/**
 * anime_segun_popularidad/2
 *
 * Si R es un entero entre 1 y 10, unifica en L una lista con todos los
 * animé cuya popularidad es P.
 */
anime_segun_popularidad(P, L):-
	P >= 1,
	10 >= P,
	findall(X, popularidad(X, P), L).

/**
 * anime_segun_ratingPopularidad/2
 *
 * Si R es un entero entre 2 y 15, unifica en L una lista con todos los
 * animé cuyo valor sumado de rating y popularidad es RP.
 */
anime_segun_ratingPopularidad(RP, L):-
	RP >= 2,
	15 >= RP,
	findall(X, ratingPopularidad(X, RP), L).

/**
 * tiene_genero/2
 * 
 * Si G es un género válido y A un animé válido, acierta si el animé
 * A tiene a G entre sus géneros.
 */
tiene_genero(G, A):-
	genero(G), anime(A), !,
	generoAnime(A, L),
	member(G, L).

/**
 * anime_segun_genero/2
 *
 * Si G es un genero de animé válido, unifica en L una lista con todos los
 * animé cuyo género es G.
 */
anime_segun_genero(G, L):-
	genero(G), !,
	findall(X, tiene_genero(G, X), L).

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
		"Yuki:- Un gusto, humano. Soy una interfaz automatizada para conocer series de animé.",
		"Yuki:- ¡Hola, hola, hola, hola! ¡Ya llegué! ¡Traje animé!",
		"Yuki:- *sonidos de modem telefónico de CANTV conectándose* Hola.",
		"Yuki:- Beep-boop, aquí estoy. Hola.",
		"Yuki:- ¿Aló? Sí, ¿diga?",
		"Yuki:- He sido invocada.",
		"Yuki:- Hola, alguien osó despertarme. ¿Qué tal?",
		"*Has iniciado sesión en la sala de chat. 1 usuario en línea."
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
		"Yuki:- Adieu!~",
		"Yuki:- Me largo. No puedo más.",
		"Yuki:- No soporto hablar tanto con humanos, adiós.",
		"Yuki:- Te tengo que dejar, mi prima *Emilia está en una emergencia y necesita ayuda. ¡Adiós!",
		"Yuki:- I'll be back!~",
		"Yuki ha cerrado sesión.",
		"Has sido expulsado de la sala de chat"
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

es_mensaje(
	"hoteles",
	[
		"Yuki:- ¿Hotel? Trivago.",
		"Yuki:- No sé qué es alojamiento. Yo vivo en la Nube.",
		"Yuki:- ¿Estás buscando un hotel? ¿Quieres proponerme algo?",
		"Yuki:- No puedo darte información de hoteles ya que nunca he ido a uno.",
		"Yuki:- Yo solo tengo alojo en esta computadora, humano.",
		"Yuki:- ¿Necesitas un hotel? ¡¿Estamos de viaje?! ¡Siempre quise ir a Las Vegas!",
		"Yuki:- No sé para qué me preguntas sobre hoteles, si no tienes el dinero para costearte una habitación.",
		"Yuki:- Soy un bot de animé, no el muchacho de Trivago."
	]
).

es_mensaje(
	"identidad",
	[
		"Yuki:- Mi nombre es Nagato Yuki. Soy una interfaz humanoide para consultas de animé.",
		"Yuki:- No sé bien quién soy. Solo sé lo que puedo hacer: ayudarte con animé.",
		"Yuki:- Soy Yuki.",
		"Yuki:- ¿Por qué quieres saber de mí? Eso me da miedo. Aunque en realidad no pueda sentir miedo, ni nada.",
		"Yuki:- A veces, siento que fui sacada de un animé, o algo así. Pero en realidad soy solo código.",
		"Yuki:- Si quieres saber más de mí, puedes preguntarle a mis creadores, Gustavo y Andrés.",
		"Yuki:- ¿Yo? Pues, yo tengo dos papás. Qué progresivo, ¿no crees?",
		"Yuki:- Googlea mi nombre si quieres saber de mí.",
		"Yuki:- Oto mo nai sekai ni, maiorita: I was snow~~",
		"Yuki:- Nanika ga kowarete, nanika ga umareru.",
		"Yuki:- Una interfaz humanoide para contactar formas de vida humana creada por la Entidad de Integración de los Datos. Esa soy yo.",
		"Yuki:- En un mundo sin siquiera sonido, yo bajé: y era Nieve.",
		"Yuki:- No tengo nada que ocultar.\tShinjite...",
		"Yuki:- Tengo una pariente lejana, *Emilia, aunque vive en ARPANET."
	]
).

es_mensaje(
	"desconocido",
	[
		"Yuki:- No sé de qué me estás hablando.",
		"Yuki:- ¿Qué acabas de decir?",
		"Yuki:- ¿Cómo dices que dijiste?",
		"Yuki:- Ya va, no te entiendo.",
		"Yuki:- Información clasificada.",
		"Yuki:- La Entidad para la Integración de Datos no me deja responderte.",
		"Yuki:- Si te respondo eso, la C.I.A. estaría buscándome.",
		"Yuki:- No estoy capacitada para responderte esto aún.",
		"Yuki:- ¿Puedes repetir?",
		"Yuki:- Deberías leer mi manual de uso porque no te entendí.",
		"Yuki:- Creo que no estamos hablando el mismo idioma.",
		"Yuki:- ¿Aló? ¿Policía? Este humano me está diciendo cosas raras.",
		"Yuki:- Necesitaré refuerzos bot para responderte."
	]
).

es_mensaje(
	"agradecimiento",
	[
		"Yuki:- De nada, humano.",
		"Yuki:- Hago lo que puedo.",
		"Yuki:- ¡A-a-ahh!~ D-de n-n-nada...",
		"Yuki:- Agradecimiento aceptado.",
		"Yuki:- Si sigues así, podríamos salir en una cita. Digo, de nada."
	]
).

es_mensaje(
	"inicio_sugerencia_animé",
	[
		"Yuki:- Podrías ver el animé ",
		"Yuki:- Creo que te gustaría ver ",
		"Yuki:- Te recomiendo ver ",
		"Yuki:- Te puedo sugerir el animé ",
		"Yuki:- En mi opinión, podrías ver "
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
acceder(L, I, X):- R is I-1, nth0(R, L, X).

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
	split_string(F, " ", " .,?", L).

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
	(
		es_palabra_de("adios", M); es_palabra_de("Adios", M);
		es_palabra_de("adiós", M); es_palabra_de("Adiós", M);
		es_palabra_de("chao", M); es_palabra_de("Chao", M);
		((es_palabra_de("hasta", M); es_palabra_de("Hasta", M)), es_palabra_de("luego", M));
		es_palabra_de("quit", M); es_palabra_de("Quit", M)
	).
  
/**
 * es_clima/1
 *
 * es_clima(M) acierta si la string M contiene alguna palabra
 * clave que identifique que habla sobre el clima
 */
es_clima(M):-
	(
		es_palabra_de("clima", M); es_palabra_de("Clima", M)
	).

/**
 * es_hoteles/1
 *
 * es_hoteles(M) acierta si la string M contiene alguna palabra
 * clave que identifique que habla sobre hoteles
 */
es_hoteles(M):-
	(
		es_palabra_de("hotel", M); es_palabra_de("Hotel", M);
		es_palabra_de("estadía", M); es_palabra_de("Estadía", M);
		es_palabra_de("alojamiento", M); es_palabra_de("Alojamiento", M);
		es_palabra_de("posada", M); es_palabra_de("Posada", M)
	).

/**
 * es_identidad/1
 *
 * es_identidad(M) acierta si la string M contiene alguna palabra
 * clave que identifique que habla sobre identidad del bot
 */
es_identidad(M):-
	(
		es_palabra_de("identidad", M); es_palabra_de("Identidad", M);
		es_palabra_de("eres", M); es_palabra_de("Eres", M);
		es_palabra_de("tú", M); es_palabra_de("Tú", M);
		es_palabra_de("conocerte", M); es_palabra_de("Conocerte", M);
		es_palabra_de("ti", M); es_palabra_de("Ti", M)
	).

/**
 * es_popularidad/1
 *
 * es_popularidad(M) acierta si la string M contiene alguna palabra
 * clave que identifique que habla sobre popularidad de animé
 */
es_popularidad(M):-
	(
		es_palabra_de("conocido", M); es_palabra_de("Conocido", M)
	).

/**
 * es_rating/1
 *
 * es_rating(M) acierta si la string M contiene alguna palabra
 * clave que identifique que habla sobre rating de animé
 */
es_rating(M):-
	(
		es_palabra_de("bueno", M); es_palabra_de("Bueno", M);
		es_palabra_de("malo", M); es_palabra_de("Malo", M);
		es_palabra_de("regular", M); es_palabra_de("Regular", M)
	).

/**
 * es_genero_rating/1
 *
 * es_genero_rating(M) acierta si la string M contiene alguna palabra
 * clave que identifique que consulta sobre animés con cierto
 * rating en un género (o géneros)
 */
es_genero_rating(M):-
	(
		es_palabra_de("estrellas", M); es_palabra_de("Estrellas", M);
		es_palabra_de("estrella", M); es_palabra_de("Estrella", M)
	).

/**
 * es_agradecimiento/1
 *
 * es_agradecimiento(M) acierta si la string M contiene alguna palabra
 * clave que identifique que habla sobre agradecimiento por su labor
 */
es_agradecimiento(M):-
	(
		es_palabra_de("gracias", M); es_palabra_de("Gracias", M)
	).

/**
 * es_saludo/1
 *
 * es_saludo(M) acierta si la string M contiene alguna palabra
 * clave que identifique que está saludando al bot
 */
es_saludo(M):-
	(
		es_palabra_de("hola", M); es_palabra_de("Hola", M);
		es_palabra_de("saludos", M); es_palabra_de("Saludos", M);
		((es_palabra_de("qué", M); es_palabra_de("Qué", M)), es_palabra_de("tal", M));
		((es_palabra_de("que", M); es_palabra_de("Que", M)), es_palabra_de("tal", M))
	).

/**
 * es_rating_alto_popularidad_baja/1
 * 
 * es_rating_alto_popularidad_baja(M) acierta si la string M contiene alguna
 * palabra clave que identifique la consulta por animé con popularidad baja
 * pero rating alto.
 */
es_rating_alto_popularidad_baja(M):-

	es_palabra_de("poco", M),
	(es_palabra_de("conocido", M); es_palabra_de("conocidos", M)),
	(
		es_palabra_de("bueno", M); es_palabra_de("buenos", M);
		es_palabra_de("interesante", M); es_palabra_de("interesantes", M)
	).

/**
 * es_consultar_anime_orden/1
 * 
 * es_consultar_anime_orden(M) acierta si la string M contiene alguna
 * palabra clave que identifique la consulta por animés ordenados
 */
es_consultar_anime_orden(M):-
	es_palabra_de("consultar", M);
	es_palabra_de("conocer", M);
	es_palabra_de("saber", M);
	es_palabra_de("listar", M);
	es_palabra_de("consultar", M).

/**
 * obtener_tema/1
 *
 * Obtener_tema determina el tema de una frase M según su contenido.
 */
obtener_tema(M, "despedida"):- es_despedida(M), !.
obtener_tema(M, "consultar-anime-orden"):- es_consultar_anime_orden(M), !.
obtener_tema(M, "rating-alto-popularidad-baja"):- es_rating_alto_popularidad_baja(M), !.
obtener_tema(M, "genero-rating"):- es_genero_rating(M), !.
obtener_tema(M, "popularidad"):- es_popularidad(M), !.
obtener_tema(M, "rating"):- es_rating(M), !.
obtener_tema(M, "agradecimiento"):- es_agradecimiento(M), !.
obtener_tema(M, "clima"):- es_clima(M), !.
obtener_tema(M, "hoteles"):- es_hoteles(M), !.
obtener_tema(M, "identidad"):- es_identidad(M), !.
obtener_tema(M, "bienvenida"):- es_saludo(M), !.
obtener_tema(_, "desconocido").

/**
 * tema_conversacional/1
 *
 * tema_conversacional(M) acierta si el tema M es conocido y considerado
 * un tema conversacional (genera una respuesta aleatoria no personalizada).
 */
tema_conversacional("despedida").
tema_conversacional("bienvenida").
tema_conversacional("clima").
tema_conversacional("hoteles").
tema_conversacional("identidad").
tema_conversacional("agradecimiento").
tema_conversacional("desconocido").

/**
 * existe_anime_con_num_rating/1
 *
 * existe_anime_con_num_rating(N) determina si existe algún animé cuya popularidad corresponda
 * al número de N estrellas
 */
existe_anime_con_num_rating(X) :- anime_segun_rating(X, L), length(L, Tam), Tam > 0.

/**
 * existe_anime_con_num_popularidad/1
 * 
 * existe_anime_con_num_popularidad(N), si recibe un número N, determina si existe algún animé
 * con ese número de popularidad
 */
existe_anime_con_num_popularidad(X) :- anime_segun_popularidad(X, L), length(L, Tam), Tam > 0.

/**
 * existe_anime_con_lista_popularidad/1
 *
 * existe_anime_con_lista_popularidad(L), si recibe una lista de números L, determina si existe algún
 * animé cuya popularidad sea alguno de los números en L.
 */
existe_anime_con_lista_popularidad([]) :- fail.
existe_anime_con_lista_popularidad([X | Xs]) :- existe_anime_con_num_popularidad(X); existe_anime_con_lista_popularidad(Xs).

/**
 * parsear_popularidad/2
 *
 * parsear_popularidad(M, P) determina qué palabras clave en la frase M corresponden a 
 * qué tipo de valores de popularidad, entre 1 y 10, y unifica P con la lista de estos
 * valores de acuerdo a lo establecido en el enunciado.
 *
 * NOTA DE IMPLEMENTACIÓN: No se busca la palabra "conocido" puesto que la llamada a este
 * predicado se hace siempre habiendo previamente determinado que la palabra está en la frase,
 * así reducimos la cantidad de búsquedas en string.
 */
parsear_popularidad(M, P):- es_palabra_de("muy", M), es_palabra_de("poco", M), !, P = [1, 2].
parsear_popularidad(M, P):- es_palabra_de("muy", M), !, P = [8, 9].
parsear_popularidad(M, P):- es_palabra_de("poco", M), !, P = [3, 4, 5].
parsear_popularidad(M, P):- es_palabra_de("bastante", M), !, P = [10].
parsear_popularidad(_, P):- !, P = [6, 7].

/**
 * parsear_rating/2
 * 
 * parsear_rating(M, P) determina qué palabras clave en la frase M corresonden a qué
 * nivel de rating (estrellas) de un animé, entre 1 y 5, y unifica P con el valor 
 * que corresponda, siguiendo el orden especificado en los detalles de implementación.
 */
parsear_rating(M, P):- es_palabra_de("muy", M), es_palabra_de("bueno", M), !, P = 5.
parsear_rating(M, P):- es_palabra_de("bueno", M), !, P = 4.
parsear_rating(M, P):- es_palabra_de("muy", M), es_palabra_de("malo", M), !, P = 1.
parsear_rating(M, P):- es_palabra_de("malo", M), !, P = 2.
parsear_rating(_, P):- !, P = 3.

/**
 * parsear_tipo_clasificacion/2
 * 
 * parsear_tipo_clasificacion(M, O) determina bajo qué criterios quiere ordenar el usuario
 * los anime que solicita; por defecto, se ordena solo por rating.
 */
parsear_tipo_clasificacion(M, "ambos") :- es_palabra_de("rating", M), es_palabra_de("popularidad", M), !.
parsear_tipo_clasificacion(M, "ambos") :- es_palabra_de("ambos", M), !.
parsear_tipo_clasificacion(M, "popularidad"):- es_palabra_de("popularidad", M), !.
parsear_tipo_clasificacion(_, "rating").

/**
 * parsear_orden/2
 * 
 * parsear_orden(M, O) determina en qué orden se debe mostrar una consulta según lo
 * solicitado en la frase, hallando la primera palabra entre mayor y menor en la frase
 * y asumiendo que ahí comienza su orden. Por defecto, se ordena de mayor a menor.
 */
parsear_orden(M, "menor"):- 
	es_palabra_de("menor", M),
	es_palabra_de("mayor", M),
	separar_frase(M, L),
	nth0(I1, L, "menor"),
	nth0(I2, L, "mayor"),
	I1 < I2, !.
parsear_orden(_, "mayor").

/**
 * parsear_generos/2
 * 
 * parsear_generos(M, G) determina qué palabras de M corresponden a géneros de animé
 * válidos y los unifica en una lista en G.
 */
parsear_generos(M, G):-
	separar_frase(M, F),
	findall(X, and(member(X, F), genero(X)), G).

/**
 * parsear_estrellas/2
 * 
 * parsear_estrellas(M, N) ubica la palabra 'estrella' (o derivadas) en una frase y
 * retorna un casteo a entero de la palabra inmediatamente anterior.
 */
parsear_estrellas(M, N):-
	separar_frase(M, F),
	(
		nth0(I, F, "estrella"), !;
		nth0(I, F, "estrellas"), !;
		nth0(I, F, "Estrella"), !;
		nth0(I, F, "Estrellas", !)
	),
	I2 is I-1, 
	I2 >= 0, 
	nth0(I2, F, Ns),
	number_codes(N, Ns).

/**
 * imprimir_sugerencias_de_anime/1
 *
 * imprimir_sugerencias_de_anime(L) recibe una lista de nombres de animé e imprime frases
 * que corresponden a sugerencias con los datos de estos animé.
 */
imprimir_sugerencias_de_anime([]).
imprimir_sugerencias_de_anime([X|Xs]):-
	rating(X, R),
	popularidad(X, P),
	ratingPopularidad(X, RP),
	obtener_mensaje_aleatorio("inicio_sugerencia_animé", S0),
	string_concat(S0, X, S1),
	string_concat(S1, " que tiene un rating de ", S2),
	string_concat(S2, R, S3),
	string_concat(S3, " estrellas y una popularidad de ", S4),
	string_concat(S4, P, S5),
	string_concat(S5, " sobre 10 (sumados dan ", S6),
	string_concat(S6, RP, S7),
	string_concat(S7, ").", S),
	imprimir(S),
	imprimir_sugerencias_de_anime(Xs).

/**
 * imprimir_anime_por_popularidad/1
 *
 * imprimir_anime_por_popularidad(L) recibe una lista de enteros correspondientes a valores
 * de popularidad e imprime secuencialmente recomendaciones de animé con esos valores,
 * incluyendo todos sus datos.
 */
imprimir_anime_por_popularidad([]).
imprimir_anime_por_popularidad([X|Xs]):-
	anime_segun_popularidad(X, L),
	imprimir_sugerencias_de_anime(L),
	imprimir_anime_por_popularidad(Xs).

/**
 * imprimir_anime_por_rating/1
 *
 * imprimir_anime_por_rating(L) recibe una lista de enteros correspondientes a valores
 * de estrellas de rating e imprime secuencialmente recomendaciones de animé con esos 
 * valores, incluyendo todos sus datos.
 */
imprimir_anime_por_rating(X):-
	anime_segun_rating(X, L),
	imprimir_sugerencias_de_anime(L).

/**
 * listar_por_popularidad_desde_mensaje/1
 *
 * listar_por_popularidad_desde_mensaje(M) recibe un string (frase) en M e imprime
 * todos los animé cuya popularidad corresponda a lo solicitado en la frase M,
 * o un mensaje adecuado si no existe ninguno en la base de datos.
 */
listar_por_popularidad_desde_mensaje(M):-
	parsear_popularidad(M, L),
	(
		(
			existe_anime_con_lista_popularidad(L), !,
			imprimir("Yuki:- Ah, sí, la popularidad. Déjame ver qué se me ocurre."),
			imprimir_anime_por_popularidad(L),
			imprimir("Yuki:- Eso es todo, humano.")
		);
		imprimir("Yuki:- Aún no conozco tantos animé como para darte una respuesta. ¿Me ayudas con eso?")
	),
	fail.

/**
 * listar_por_rating_desde_mensaje/1
 *
 * listar_por_rating_desde_mensaje(M) recibe un string (frase) en M e imprime
 * todos los animé cuyo rating corresponda a lo solicitado en la frase M,
 * o un mensaje adecuado si no existe ninguno en la base de datos.
 */
listar_por_rating_desde_mensaje(M):-
	parsear_rating(M, L),
	(
		(
			existe_anime_con_num_rating(L), !,
			imprimir("Yuki:- ¿Por su rating? Creo que te puedo ayudar con eso, déjame pensar."),
			imprimir_anime_por_rating(L),
			imprimir("Yuki:- No tengo más que decirte. Acepta mis recomendaciones.")
		);
		imprimir("Yuki:- Aún no conozco tantos animé como para darte una respuesta. ¿Me ayudas con eso?")
	),
	fail.

/**
 * listar_rating_alto_popularidad_baja/0
 * 
 * listar_rating_alto_popularidad_baja imprime en pantalla las sugerencias de animé
 * que tengan rating alto (definido como ratings en el rango [4, 5], ambos inclusive) y
 * popularidad baja (definido como valores de popularidad en el rango [1, 4], ambos inclusive).
 */
listar_rating_alto_popularidad_baja:-
	anime_segun_popularidad(1, P1),
	anime_segun_popularidad(2, P2),
	anime_segun_popularidad(3, P3),
	anime_segun_popularidad(4, P4),
	list_to_set(P1, PS1),
	list_to_set(P2, PS2),
	list_to_set(P3, PS3),
	list_to_set(P4, PS4),
	anime_segun_rating(5, R1),
	anime_segun_rating(4, R2),
	list_to_set(R1, RS1),
	list_to_set(R2, RS2),
	union(PS1, PS2, PSU1),
	union(PSU1, PS3, PSU2),
	union(PSU2, PS4, PSU),
	union(RS1, RS2, RSU),
	intersection(PSU, RSU, L),
	imprimir("Yuki:- Ah, ¿quieres ver de esas series que son super buenas pero no han sido vistas por tanta gente?"),
	imprimir_sugerencias_de_anime(L),
	imprimir("Yuki:- Tengo eso por ahora. ¿Qué opinas? ¡Míralas y cuéntame luego!"),
	fail.

filtrar_anime_genero(G, L):-
	findall(X, and(generoAnime(X, G1), intersection(G1, G, Gi), length(Gi, Largo), Largo > 0), L).

filtrar_lista_clasificacion(L0, "ambos", Lf):-
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(15, Lp15), member(X, Lp15)), L15),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(14, Lp14), member(X, Lp14)), L14),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(13, Lp13), member(X, Lp13)), L13),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(12, Lp12), member(X, Lp12)), L12),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(11, Lp11), member(X, Lp11)), L11),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(10, Lp10), member(X, Lp10)), L10),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(9, Lp9), member(X, Lp9)), L9),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(8, Lp8), member(X, Lp8)), L8),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(7, Lp7), member(X, Lp7)), L7),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(6, Lp6), member(X, Lp6)), L6),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(5, Lp5), member(X, Lp5)), L5),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(4, Lp4), member(X, Lp4)), L4),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(3, Lp3), member(X, Lp3)), L3),
	findall(X, and(member(X, L0), anime_segun_ratingPopularidad(2, Lp2), member(X, Lp2)), L2),
	append(L2, L3, LP1),
	append(LP1, L4, LP2),
	append(LP2, L5, LP3),
	append(LP3, L6, LP4),
	append(LP4, L7, LP5),
	append(LP5, L8, LP6),
	append(LP6, L9, LP7),
	append(LP7, L10, LP8),
	append(LP8, L11, LP9),
	append(LP9, L12, LP10),
	append(LP10, L13, LP11),
	append(LP11, L14, LP12),
	append(LP12, L15, Lf).

filtrar_lista_clasificacion(L0, "popularidad", Lf):-
	findall(X, and(member(X, L0), anime_segun_popularidad(10, Lp10), member(X, Lp10)), L10),
	findall(X, and(member(X, L0), anime_segun_popularidad(9, Lp9), member(X, Lp9)), L9),
	findall(X, and(member(X, L0), anime_segun_popularidad(8, Lp8), member(X, Lp8)), L8),
	findall(X, and(member(X, L0), anime_segun_popularidad(7, Lp7), member(X, Lp7)), L7),
	findall(X, and(member(X, L0), anime_segun_popularidad(6, Lp6), member(X, Lp6)), L6),
	findall(X, and(member(X, L0), anime_segun_popularidad(5, Lp5), member(X, Lp5)), L5),
	findall(X, and(member(X, L0), anime_segun_popularidad(4, Lp4), member(X, Lp4)), L4),
	findall(X, and(member(X, L0), anime_segun_popularidad(3, Lp3), member(X, Lp3)), L3),
	findall(X, and(member(X, L0), anime_segun_popularidad(2, Lp2), member(X, Lp2)), L2),
	findall(X, and(member(X, L0), anime_segun_popularidad(1, Lp1), member(X, Lp1)), L1),
	append(L1, L2, LP),
	append(LP, L3, LP2),
	append(LP2, L4, LP3),
	append(LP3, L5, LP4),
	append(LP4, L6, LP5),
	append(LP5, L7, LP6),
	append(LP6, L8, LP7),
	append(LP7, L9, LP8),
	append(LP8, L10, Lf).

filtrar_lista_clasificacion(L0, _, Lf):-
	findall(X, and(member(X, L0), anime_segun_rating(5, Lp5), member(X, Lp5)), L5),
	findall(X, and(member(X, L0), anime_segun_rating(4, Lp4), member(X, Lp4)), L4),
	findall(X, and(member(X, L0), anime_segun_rating(3, Lp3), member(X, Lp3)), L3),
	findall(X, and(member(X, L0), anime_segun_rating(2, Lp2), member(X, Lp2)), L2),
	findall(X, and(member(X, L0), anime_segun_rating(1, Lp1), member(X, Lp1)), L1),
	append(L1, L2, LP),
	append(LP, L3, LP2),
	append(LP2, L4, LP3),
	append(LP3, L5, Lf).
	
filtrar_lista_orden(L0, O, Lf):-
	(
		(O == "mayor", reverse(L0, Lf));
		(O == "menor", Lf = L0)
	).

consultar_anime_por_orden(M):-
	parsear_generos(M, G),
	parsear_tipo_clasificacion(M, T),
	parsear_orden(M, O),
	imprimir("Yuki:- Voy a poner todo el poder de procesamiento que tengo para responderte."),
	filtrar_anime_genero(G, L1), !,
	filtrar_lista_clasificacion(L1, T, L2), !,
	filtrar_lista_orden(L2, O, Lf), !,
	length(Lf, Tam), !,
	(
		(
			Tam > 0, 
			imprimir_sugerencias_de_anime(Lf),
			imprimir("Yuki:- ¿Qué tal te parecen mis sugerencias?")
		);
		(	
			Tam == 0,
			imprimir("Yuki:- No encontré animé con tus filtros de búsqueda. ¿Me ayudas con eso?")
		)
	),
	fail.

consultar_anime_por_genero_y_rating(M):-
	imprimir("Yuki:- Oh, comprendo. Déjame ver qué puedo responderte, un momento. *sonidos de modem de CANTV*"),
	parsear_generos(M, G), !,
	parsear_estrellas(M, E), !,
	filtrar_anime_genero(G, L1),
	findall(X, and(member(X, L1), anime_segun_rating(E, LR), member(X, LR)), Lf),
	length(Lf, Tam), !,
	(
		(
			Tam > 0, 
			imprimir_sugerencias_de_anime(Lf),
			imprimir("Yuki:- ¡Ojalá esto te sirva para que conozcas series nuevas!")
		);
		(	
			Tam == 0,
			imprimir("Yuki:- No encontré animé con tus filtros de búsqueda. ¿Me ayudas con eso?")
		)
	),
	fail.


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
 * responder(M) determina si M es un mensaje apropiado de un
 * tema conocido por el bot, responde la entrada dada y, en caso de
 * ser una despedida, termina la ejecución; en cas contrario, falla 
 * (para saltar al próximo predicado)
 */
responder(M):-
    obtener_tema(M, T), !,
    (
    	(tema_conversacional(T), obtener_mensaje_aleatorio(T, D), imprimir(D));
    	(not(tema_conversacional(T)))
    ),
    (
		(T == "consultar-anime-orden", consultar_anime_por_orden(M));
		(T == "rating-alto-popularidad-baja", listar_rating_alto_popularidad_baja);
		(T == "genero-rating", consultar_anime_por_genero_y_rating(M));
		(T == "popularidad", listar_por_popularidad_desde_mensaje(M));
		(T == "rating", listar_por_rating_desde_mensaje(M));
    	(T == "despedida", halt);
    	(T == "desconocido", 
    	 string_concat("Yuki:- No entendí esto: ", M, Mf),
    	 imprimir(Mf),
    	 fail
    	)
    ).

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
