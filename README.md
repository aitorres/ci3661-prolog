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

## Detalles de Implementación

Esta sección describe algunos detalles de la implementación del bot de animé, decisiones de estructura y diseño, cambios con respecto a lo solicitado en el enunciado (si aplica), entre otros.

### Nombre del archivo

El enunciado del proyecto requiere de un archivo `AniBot.pl` que maneje la lógica de uso del chatbot. Sin embargo, SWI-Prolog genera un error por el nombre de archivo debido a las mayúsculas en el mismo. Por lo tanto, se renombró el archivo a `anibot.pl`, evitando generar dicho error.

### Manejo de frases

Se garantiza el reconocimiento de las frases especificadas en este documento **siempre y cuando** se utilicen minúsculas, preferiblemente en toda la frase.

**Nota**: A menos que explícitamente se indique alguna restricción, este documento considera *input*, *consulta* y *pregunta* como lo mismo: un mensaje que el usuario introduce en el prompt de la conversación con el bot.

### Aleatoriedad en respuestas

Algunas de las posibles respuestas del bot se obtienen de manera aleatoria de una base de datos de respuestas de acuerdo al tema, para darle más dinamismo y cierta personalidad. La lista completa de mensajes puede encontrarse en las definiciones de las reglas `es_mensaje` en [anibot.pl](anibot.pl).

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

### Temas adicionales

Además de estar facultado para dar detalles y responder a consultas sobre temas relacionados a series de animé, el bot está (medianamente) facultado para responder (jocosamente) a consultas sobre:

- El clima
- Hoteles
- Su identidad
- Chistes
- Otros bots
- Saludos y despedidas
- Temas desconocidos

@TODO: Describir más

## Otras observaciones

### Sobre este documento

La maquetación de este documento fue realizada en *Markdown*, específicamente el dialecto de *Markdown* enriquecido para su uso en *Github*.

@TODO: Escribir, si hacen falta