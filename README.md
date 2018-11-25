# AniBot
Proyecto de programación lógica en Prolog para la asignatura Laboratorio de Lenguajes de Programación (CI-3661) en la Universidad Simón Bolívar

## Equipo
1. Gustavo Castellanos, 14-10192
2. Andres Torres, 14-11082

## Enunciado
@TODO: Colocar link

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