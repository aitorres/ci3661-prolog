# AniBot

Proyecto de programación lógica en Prolog para la asignatura Laboratorio de Lenguajes de Programación (CI-3661) en la Universidad Simón Bolívar

## Equipo

1. Gustavo Castellanos, 14-10192
2. Andres Torres, 14-11082

## Enunciado

[Ver aquí: *ci3661-prolog-enunciado.pdf* en Google Drive](https://drive.google.com/file/d/1FtN-zCX_xXkFdX_0rxv7aP-tX7jtOcmM/view?usp=sharing)

## Modo de uso

El bot puede utilizarse dentro de un intérprete del dialecto SWI-Prolog en cualquier sistema operativo compatible. Para esto, primero se debe ejecutar el intérprete de prolog en la misma carpeta del proyecto. En Linux, esto podría realizarse así:

```bash
swipl
```

Posteriormente, dentro del intérprete de prolog, cargar el archivo:

```swipl
[anibot].
```

Alternativamente, se puede realizar la carga del archivo desde el terminal de comandos, iniciando el intérprete `swipl` con el flag `-s` de la siguiente manera (de nuevo, en la misma carpeta del proyecto):

```bash
swipl -s anibot.pl
```

Al realizar la carga satisfactoriamente (se lee **true** en la pantalla), basta con llamar al predicado `chat/0` para interactuar con el bot.

```swipl
chat.
```

El flujo de interacción con el bot corresponde a mensajes (preguntas, consultas) del usuario y respuestas del bot. Los mensajes del bot vienen identificados con el prompt *Yuki:-*, mientras que los mensajes del usuario vienen identificados con el prompt *Yo:-*. Si en pantalla se muestra el prompt *Yo:-*, el programa está a la espera de un mensaje del usuario.

Para terminar la ejecución, usted debe despedirse del bot. Por ejemplo, decir algo como:

```swipl
Yo:- Adiós, bot.
```

Sea educado con el bot ;-)

## Listas de género y animé

Por defecto, el chatbot reconoce en su base de datos los siguientes géneros:

- Aventura
- Shoujo
- Shounen
- Kodomo
- Seinen
- Josei
- Ficción
- Fantasía
- Mecha
- Sobrenatural
- Magia
- Gore

De igual forma, por defecto, tiene conocimiento de las siguientes series de animé:

- Dragon Ball
- Naruto
- Bleach
- HunterXHunter
- Hamtaro
- Full Metal Alchemist
- Suzumiya Haruhi No Yuutsu
- Sword Art Online
- Another
- Death Note
- Attack Oon Titan
- Steins;Gate
- Pokémon
- InuYasha
- Kuroshitsuji
- Yu-Gi-Oh!
- Digimon
- Eureka Seven
- School Days
- Free!
- Cowboy Bebop
- Planet Survival
- Noir
- Gundam
- Accel World

Cada animé cuenta con su información asociada de género o géneros (hasta 5), rating y popularidad, en función de lo establecido por el cuerpo profesional, reseñas de internet y apreciación personal de los estudiantes.

## Detalles de Implementación

Esta sección describe algunos detalles de la implementación del bot de animé, decisiones de estructura y diseño, cambios con respecto a lo solicitado en el enunciado (si aplica), entre otros.

**NOTA IMPORTANTE**: A menos que se indique de manera más explícita, las palabras clave / palabras reservadas / tokens que activan cada funcionalidad en el chatbot se muestran en *cursivas* en este documento.

### Nombre del archivo

El enunciado del proyecto requiere de un archivo `AniBot.pl` que maneje la lógica de uso del chatbot. Sin embargo, SWI-Prolog genera un error por el nombre de archivo debido a las mayúsculas en el mismo. Por lo tanto, se renombró el archivo a `anibot.pl`, evitando generar dicho error.

### Manejo de frases

Se garantiza el reconocimiento de las frases especificadas en este documento **siempre y cuando** se utilicen minúsculas, preferiblemente en toda la frase.

**Nota**: A menos que explícitamente se indique alguna restricción, este documento considera *input*, *consulta* y *pregunta* como lo mismo: un mensaje que el usuario introduce en el prompt de la conversación con el bot.

### Aleatoriedad en respuestas

Algunas de las posibles respuestas del bot se obtienen de manera aleatoria de una base de datos de respuestas de acuerdo al tema, para darle más dinamismo y cierta personalidad. La lista completa de mensajes puede encontrarse en las definiciones de las reglas `es_mensaje` en [anibot.pl](anibot.pl).

### Agregar un nuevo animé

El bot permite agregar nuevos animés a la lista de animé en su base de datos de manera dinámica. La palabra clave de este tipo de consultas es *nuevo*, y la sintaxis debe seguir la siguiente (de ejemplo):

`Conozco un nuevo animé llamado Kimi No Nawa de 4 estrellas y de tipo Fantasía`

Opcionalmente, se puede reconocer la popularidad:

`Conozco un nuevo animé llamado Kimi No Nawa de 4 estrellas y de tipo Fantasía de popularidad 7`

Un ejemplo (tomado de la ejecución del programa) sigue:

```swipl
?- chat.
Yuki:- Hola, soy Yuki. ¿Quieres hablar?
Yo:- Aja mira, conocí este nuevo anime llamado Dragon Ball GT con 3 estrellas y de tipo Shounen
Yuki:- Okay, ahora recordaré Dragon Ball GT
Yo:- Dame un animé bastante conocido.
Yuki:- Ah, sí, la popularidad. Déjame ver qué se me ocurre.
Yuki:- En mi opinión, podrías ver Full Metal Alchemist que tiene un rating de 4 estrellas y una popularidad de 1 sobre 10 (sumados dan 5).
Yuki:- Creo que te gustaría ver Dragon Ball GT que tiene un rating de 3 estrellas y una popularidad de 1 sobre 10 (sumados dan 4).
Yuki:- Te recomiendo ver Eureka Seven que tiene un rating de 3 estrellas y una popularidad de 2 sobre 10 (sumados dan 5).
Yuki:- Eso es todo, humano.
```

### Consultas de animé por género, tipo de consulta y orden

El bot está facultado para responder ante consultas para listar animés que pertenezcan algún género dado, siguiendo un orden dado en enumeración y con algún criterio de orden.

Se pueden seleccionar como **órdenes de enumeración**:

- De mayor a menor
- De menor a mayor

Por defecto, se selecciona de mayor a menor.

Se pueden seleccionar como **criterios de órden**:

- Por rating
- Por popularidad
- Ambos (entendido como *rating y popularidad*, ó *ambos* criterios)

Se pueden solicitar sugerencias de uno o varios géneros de animé. En caso de que se introduzcan varios, se consultará de manera inclusiva: si un animé pertenece a *alguno* de esos géneros, se mostrará en las recomendaciones.

**NOTA IMPORTANTE**: Para poder ejecutar esta consulta, la frase a utilizar debe contener las palabras clave *conocer*, *consultar*, *listar* y/o *saber*. Es una de las consultas más críticas y estrictas en su *phrasing*.

**NOTA IMPORTANTE**: Para que el bot reconozca un género, debe escribirse *TAL CUAL* figura en la lista de géneros, con mayúsculas y acentos si aplica.

A continuación se muestra una conversación con varios ejemplos de uso (tomado de la ejecución del programa, con líneas y espacios en blanco agregada para facilitar legibilidad):

```swipl
?- chat.
Yuki:- ¿Aló? Sí, ¿diga?
Yo:- Sí, vamos al grano. Quiero conocer de animé Shounen por rating de menor a mayor.

Yuki:- Voy a poner todo el poder de procesamiento que tengo para responderte.
Yuki:- Creo que te gustaría ver Naruto que tiene un rating de 1 estrellas y una popularidad de 5 sobre 10 (sumados dan 6).
Yuki:- Te recomiendo ver Dragon Ball que tiene un rating de 3 estrellas y una popularidad de 7 sobre 10 (sumados dan 10).
Yuki:- Creo que te gustaría ver Yu-Gi-Oh! que tiene un rating de 3 estrellas y una popularidad de 7 sobre 10 (sumados dan 10).
Yuki:- Podrías ver el animé Bleach que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10 (sumados dan 12).
Yuki:- Creo que te gustaría ver Full Metal Alchemist que tiene un rating de 4 estrellas y una popularidad de 1 sobre 10 (sumados dan 5).
Yuki:- Te recomiendo ver Digimon que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10 (sumados dan 12).
Yuki:- ¿Qué tal te parecen mis sugerencias?

Yo:- Okay, ahora quiero conocer animé Sobrenatural por popularidad de mayor a menor.

Yuki:- Voy a poner todo el poder de procesamiento que tengo para responderte.
Yuki:- Creo que te gustaría ver Death Note que tiene un rating de 5 estrellas y una popularidad de 10 sobre 10 (sumados dan 15).
Yuki:- Te recomiendo ver Bleach que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10 (sumados dan 12).
Yuki:- Te puedo sugerir el animé Suzumiya Haruhi no Yuutsu que tiene un rating de 3 estrellas y una popularidad de 6 sobre 10 (sumados dan 9).
Yuki:- Te puedo sugerir el animé Another que tiene un rating de 4 estrellas y una popularidad de 5 sobre 10 (sumados dan 9).
Yuki:- Te recomiendo ver Steins;Gate que tiene un rating de 2 estrellas y una popularidad de 4 sobre 10 (sumados dan 6).
Yuki:- ¿Qué tal te parecen mis sugerencias?

Yo:- ¿Me puedes listar animé de Fantasía por rating?

Yuki:- Voy a poner todo el poder de procesamiento que tengo para responderte.
Yuki:- Podrías ver el animé Digimon que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10 (sumados dan 12).
Yuki:- Creo que te gustaría ver Pokémon que tiene un rating de 4 estrellas y una popularidad de 10 sobre 10 (sumados dan 14).
Yuki:- Podrías ver el animé Sword Art Online que tiene un rating de 4 estrellas y una popularidad de 9 sobre 10 (sumados dan 13).
Yuki:- Creo que te gustaría ver Suzumiya Haruhi no Yuutsu que tiene un rating de 3 estrellas y una popularidad de 6 sobre 10 (sumados dan 9).
Yuki:- ¿Qué tal te parecen mis sugerencias?

Yo:- Ahora quiero saber de animé Shoujo de menor a mayor por rating y popularidad.

Yuki:- Voy a poner todo el poder de procesamiento que tengo para responderte.
Yuki:- Te puedo sugerir el animé Kuroshitsuji que tiene un rating de 2 estrellas y una popularidad de 3 sobre 10 (sumados dan 5).
Yuki:- Creo que te gustaría ver InuYasha que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10 (sumados dan 12).
Yuki:- ¿Qué tal te parecen mis sugerencias?

Yo:- Finas. Chao.
Yuki:- I'll be back!~
```

### Consultas por rating alto y popularidad baja

El bot está facultado para responder ante consultas sobre animés con **rating alto** y **popularidad baja**. A efectos de implementación, y al quedar a libre decisión del programador, se tomó como **valores de rating alto** los de **4 y 5 estrellas** (ambos inclusive), y **valores de popularidad baja** los de **1 a 4 sobre 10** (ambos inclusive).

Para utilizar esta funcionalidad, se puede consultar sobre algún animé *bueno* y *poco conocido*, o *interesante* y *poco conocido*. Por ejemplo (tomado de la ejecución del programa):

```swipl
?- chat.
Yuki:- Hola, soy Yuki. ¿Quieres hablar?
Yo:- Sí. ¿Me recomiendas algún animé interesante pero poco conocido?
Yuki:- Ah, ¿quieres ver de esas series que son super buenas pero no han sido vistas por tanta gente?
Yuki:- Te recomiendo ver Full Metal Alchemist que tiene un rating de 4 estrellas y una popularidad de 1 sobre 10.
Yuki:- Te puedo sugerir el animé HunterXHunter que tiene un rating de 5 estrellas y una popularidad de 3 sobre 10.
Yuki:- Tengo eso por ahora. ¿Qué opinas? ¡Míralas y cuéntame luego!
```

### Consultas por género y rating

El bot está facultado para responder ante consultas de listado de animés de un género (o varios géneros) con alguna cantidad de estrellas. El bot reconoce este tipo de consultas cuando se pregunta por *estrellas* o por una *estrella*, esperando que se introduzca un número antes de esta palabra.

**NOTA IMPORTANTE**: Para que el bot reconozca un género, debe escribirse *TAL CUAL* figura en la lista de géneros, con mayúsculas y acentos si aplica.

Por ejemplo (tomado de la ejecución del programa):

```swipl
?- chat.
Yuki:- Hola, alguien osó despertarme. ¿Qué tal?
Yo:- Todo fino. Mira, ¿me puedes dar animé Fantasía o Shounen con 4 estrellas?
Yuki:- Oh, comprendo. Déjame ver qué puedo responderte, un momento. *sonidos de modem de CANTV*
Yuki:- Podrías ver el animé Bleach que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10 (sumados dan 12).
Yuki:- Creo que te gustaría ver Full Metal Alchemist que tiene un rating de 4 estrellas y una popularidad de 1 sobre 10 (sumados dan 5).
Yuki:- Creo que te gustaría ver Sword Art Online que tiene un rating de 4 estrellas y una popularidad de 9 sobre 10 (sumados dan 13).
Yuki:- En mi opinión, podrías ver Pokémon que tiene un rating de 4 estrellas y una popularidad de 10 sobre 10 (sumados dan 14).
Yuki:- Te puedo sugerir el animé Digimon que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10 (sumados dan 12).
Yuki:- ¡Ojalá esto te sirva para que conozcas series nuevas!
```

### Consultas por popularidad

El bot está facultado para responder ante consultas para conocer animés de acuerdo a su popularidad (indicador numérico entre 1 y 10). La manera para realizar estas consultas es mediante preguntas por algún animé que sea:

- *bastante conocido* (popularidad de 10)
- *muy conocido* (popularidad entre 8 y 9, ambos inclusive)
- *conocido* (popularidad entre 6 y 7, ambos inclusive)
- *poco conocido* (popularidad entre 3 y 5, ambos inclusive)
- *muy poco conocido* (popularidad entre 1 y 2, ambos inclusive)

Por ejemplo (tomado de la ejecución del programa):

```swipl
?- chat.
Yuki:- ¡HOLA! HABLEMOS.
Yo:- Hola, está bien :-) ¿Me recomiendas un animé que sea muy conocido?
Yuki:- Ah, sí. Déjame ver qué se me ocurre.
Yuki:- En mi opinión, podrías ver Bleach que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10.
Yuki:- Te recomiendo ver InuYasha que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10.
Yuki:- Te recomiendo ver Digimon que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10.
Yuki:- Creo que te gustaría ver Sword Art Online que tiene un rating de 4 estrellas y una popularidad de 9 sobre 10.
Yuki:- Eso es todo, humano.
```

### Consultas por rating

El bot está facultado para responder ante consultas para conocer animés de acuerdo a su rating (cantidad de estrellas). La manera para realizar estas consultas es mediante preguntas por algún animé que sea:

- *muy bueno* (5 estrellas)
- *bueno* (4 estrellas)
- *regular* (3 estrellas)
- *malo* (2 estrellas)
- *muy malo* (1 estrella)

Por ejemplo (tomado de la ejecución del programa):

```swipl
?- chat.
Yuki:- *se asoma, tímidamente* Hola...
Yo:- Hola, ¿me recomiendas un animé que sea muy, pero muy bueno?
Yuki:- Creo que te puedo ayudar con eso, déjame pensar.
Yuki:- Podrías ver el animé HunterXHunter que tiene un rating de 5 estrellas y una popularidad de 3 sobre 10.
Yuki:- Creo que te gustaría ver Death Note que tiene un rating de 5 estrellas y una popularidad de 10 sobre 10.
Yuki:- En mi opinión, podrías ver Attack on Titan que tiene un rating de 5 estrellas y una popularidad de 10 sobre 10.
Yuki:- No tengo más que decirte. Acepta mis recomendaciones.
```

### Consultas por género

El bot está facultado (no saben cuántas veces he escrito esto) para listar animés de acuerdo a su género, al recibir una consulta que liste uno o varios géneros. El bot realiza este tipo de consultas si reconoce que alguien dice que le *gusta* o le *gustan* algunos géneros, o si dice directamente que quiere saber información de un *género* o unos *géneros* (y si la consulta no coincide con alguna de las anteriores, que tienen mayor precedencia).

**NOTA IMPORTANTE**: Para que el bot reconozca un género, debe escribirse *TAL CUAL* figura en la lista de géneros, con mayúsculas y acentos si aplica.

Por ejemplo (tomado de la ejecución del programa):

```swipl
?- chat.
Yuki:- *sonidos de modem telefónico de CANTV conectándose* Hola.
Yo:- Mira, sí, me gusta la Fantasía.
Yuki:- Veamos qué puedo hacer por ti. Si me quieres ayudar, ¡alza tus manos al cielo!
Yuki:- Podrías ver el animé Suzumiya Haruhi no Yuutsu que tiene un rating de 3 estrellas y una popularidad de 6 sobre 10 (sumados dan 9).
Yuki:- Te recomiendo ver Sword Art Online que tiene un rating de 4 estrellas y una popularidad de 9 sobre 10 (sumados dan 13).
Yuki:- Te puedo sugerir el animé Pokémon que tiene un rating de 4 estrellas y una popularidad de 10 sobre 10 (sumados dan 14).
Yuki:- Te recomiendo ver Digimon que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10 (sumados dan 12).
Yuki:- ¡Pero no te encasilles en unos pocos géneros, abre tu mente!
Yo:- ¿Y qué me dices de los géneros Shounen y Mecha?
Yuki:- Veamos qué puedo hacer por ti. Si me quieres ayudar, ¡alza tus manos al cielo!
Yuki:- Te recomiendo ver Naruto que tiene un rating de 1 estrellas y una popularidad de 5 sobre 10 (sumados dan 6).
Yuki:- Creo que te gustaría ver Dragon Ball que tiene un rating de 3 estrellas y una popularidad de 7 sobre 10 (sumados dan 10).
Yuki:- Te puedo sugerir el animé Bleach que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10 (sumados dan 12).
Yuki:- Creo que te gustaría ver Full Metal Alchemist que tiene un rating de 4 estrellas y una popularidad de 1 sobre 10 (sumados dan 5).
Yuki:- En mi opinión, podrías ver Yu-Gi-Oh! que tiene un rating de 3 estrellas y una popularidad de 7 sobre 10 (sumados dan 10).
Yuki:- Podrías ver el animé Digimon que tiene un rating de 4 estrellas y una popularidad de 8 sobre 10 (sumados dan 12).
Yuki:- Te puedo sugerir el animé Eureka Seven que tiene un rating de 3 estrellas y una popularidad de 2 sobre 10 (sumados dan 5).
Yuki:- ¡Pero no te encasilles en unos pocos géneros, abre tu mente!
```

### Temas adicionales

Además de estar facultado para dar detalles y responder a consultas sobre temas relacionados a series de animé, el bot está (medianamente) facultado para responder (jocosamente) a consultas sobre:

- El clima
- Hoteles
- Su identidad
- Chistes
- Otros bots
- Saludos y despedidas
- Temas desconocidos

## Otras observaciones

### Ayuda del programa

El bot puede generar algunos mensajes de ayuda rudimentarios, muy básicos, para orientar a un usuario. Basta que en la consulta coloque las palabras *ayuda*, *ayúdame*, *help* o similares. **Note sin embargo** que la ayuda del bot es un complemento (en ocasiones jocoso) a la ayuda oficial del programa, que se encuentra en este documento.

Un ejemplo de uso de la ayuda (tomado de la ejecución del programa):

```swipl
?- chat.
Yuki:- Un gusto, humano. Soy una interfaz automatizada para conocer series de animé.
Yo:- ¡Necesito ayuda!
Yuki:- Puedes pasarme un criterio de popularidad, por ejemplo, pregúntame por animé poco conocidos.
Yo:- No, no, ayúdame más.
Yuki:- Me da fastidio ayudarte ahorita. Pregúntame otra vez.
Yo:- ¿Qué? Bot grosero. I need help!
Yuki:- Puedes preguntarme, si quieres, por un animé que sea bueno, o malo, o muy malo.
Yo:- Mmm... Está bien. Chao.
Yuki:- Ya he aprendido suficiente de ti. Puedes irte.
```

### Sobre este documento

La maquetación de este documento fue realizada en *Markdown*, específicamente el dialecto de *Markdown* enriquecido para su uso en *Github*.
