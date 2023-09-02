# El código tiene como objetivo principal combinar las predicciones de índice de sequía (SPI6)
# con 2 meses de antelación para los diferentes meses, con el fin de generar un conjunto anual.
# MODIFICAR PARA ALMACENAR LAS PREDICCIONES CON 3 Y 4 MESES DE ANTELACIÓN


# Definir los años y meses
anni = 1981:2017
mesi = rep(1:12, length(anni))

# Encontrar los índices de inicio de cada mes
mesi_start = which(mesi == 1)
mesi_1 = which(mesi == 1)

# Directorio de salida para los resultados
dir_out = '~/Chapter3-4SPAIN/Data/'

# Iterar sobre los conjuntos de datos disponibles
for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  
  # Imprimir el conjunto de datos actual
  print(dataset)
  
  # Cargar los pronósticos SPI6 para cada mes y almacenarlos en variables individuales
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_12_enero_", dataset, ".RData", sep = ""))
  enero = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_01_febrero_", dataset, ".RData", sep = ""))
  febrero = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_02_marzo_", dataset, ".RData", sep = ""))
  marzo = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_03_abril_", dataset, ".RData", sep = ""))
  abril = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_04_mayo_", dataset, ".RData", sep = ""))
  mayo = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_05_junio_", dataset, ".RData", sep = ""))
  junio = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_06_julio_", dataset, ".RData", sep = ""))
  julio = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_07_agosto_", dataset, ".RData", sep = ""))
  agosto = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_08_septiembre_", dataset, ".RData", sep = ""))
  septiembre = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_09_octubre_", dataset, ".RData", sep = ""))
  octubre = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_10_noviembre_", dataset, ".RData", sep = ""))
  noviembre = spi6pred
  load(paste("~/Chapter3-4SPAIN/Data/SPI6ESP_11_diciembre_", dataset, ".RData", sep = ""))
  diciembre = spi6pred
  
  # Crear un pronóstico combinado anual para SPI6
  spi6pred = array(data = NA, dim = c(dim(febrero)[1], dim(febrero)[2], dim(febrero)[3], dim(febrero)[4]))
  
  spi6pred[, , mesi_start, ] = enero[, , mesi_start,]
  spi6pred[, , mesi_start + 1,] = febrero[, , mesi_start + 1,]
  spi6pred[, , mesi_start + 2,] = marzo[, , mesi_start + 2,]
  spi6pred[, , mesi_start + 3,] = abril[, , mesi_start + 3,]
  spi6pred[, , mesi_start + 4,] = mayo[, , mesi_start + 4,]
  spi6pred[, , mesi_start + 5,] = junio[, , mesi_start + 5,]
  spi6pred[, , mesi_start + 6,] = julio[, , mesi_start + 6,]
  spi6pred[, , mesi_start + 7,] = agosto[, , mesi_start + 7,]
  spi6pred[, , mesi_start + 8,] = septiembre[, , mesi_start + 8,]
  spi6pred[, , mesi_start + 9,] = octubre[, , mesi_start + 9,]
  spi6pred[, , mesi_start + 10,] = noviembre[, , mesi_start + 10,]
  spi6pred[, , mesi_start + 11,] = diciembre[, , mesi_start + 11,]
  
  # Eliminar las variables de los meses individuales para liberar memoria
  rm(enero, febrero, marzo, abril, mayo, junio, julio, agosto, septiembre, octubre, noviembre, diciembre)
  
  # Reemplazar valores infinitos y NA con NA
  spi6pred[is.infinite(spi6pred)] = NA
  spi6pred[is.na(spi6pred)] = NA
  
  # Guardar el pronóstico anual combinado de SPI6 en un archivo
  save(spi6pred, file = file.path(
    dir_out,
    paste(
      "SPI6ESP_2M_",
      dataset,
      ".RData",
      sep = ""
    )
  ))
}
