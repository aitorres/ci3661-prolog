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
- Temas desconocidos

@TODO: Describir más

## Otras observaciones
@TODO: Escribir, si hacen falta