# Evaluación de la calidad del servicio al cliente en X 
# mediante grandes modelos de lenguaje: un índice reproducible de 
#la calidad de las respuestas para operadores de telecomunicaciones en España

Este repositorio contiene el framework del conjunto de datos y 
las evaluaciones de expertos utilizadas para analizar el 
**Índice de Calidad de las Respuestas / Reseñas (RQI - Review Quality Index)** 
en datos extraídos de la plataforma X. El objetivo es medir la 
calidad del servicio dado por las operadoras de Telecomunicación frente
a mensajes realizados por los clientes mediante un índice de cuatro
aspectos básicos sentimiento, empatía, resolución y personalización.

## 📂 Estructura del Repositorio

```text
├── RQI/
│   ├── posts_movistar.csv,posts_vodafone.csv,posts_orange.csv,posts_digi.csv            # Datos brutos extraídos de X (.json / .csv)
│   └── juntar_post.R   # Procedimiento de anotación de categorías:prompts, rúbrica
├   ├── datexper1.txt     # Evaluaciones cuantitativas del Experto 1
│   ├── datexper2.txt     # Evaluaciones cuantitativas del Experto 2
│   └── datexper3.txt     # Evaluaciones cuantitativas del Experto 3
└── README.md                 # Descripción general del proyecto
```

## 📊 Metodología y Componentes

### 1. Datos Extraídos de X.- Archivos .csv

### 2. Pipeline de Entrada (Prompt & Rúbrica).- Archivo .R

### 3. Evaluación de Expertos.- Archivos .txt

## 🛠️ Cómo Utilizar este Repositorio

### Prerrequisitos
Asegúrate de contar con R4.6.1 instalado para ejecutar el scripts de análisis de datos.

## 📜 Licencia
Este proyecto se distribuye bajo la Licencia MIT. Consulta el archivo `LICENSE` para obtener más detalles.