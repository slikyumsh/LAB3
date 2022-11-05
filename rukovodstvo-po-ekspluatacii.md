---
description: User Interface
---

# Руководство по эксплуатации

* Создание матрицы по двумерному массиву data

```
Matrix a(data)
```

* Создание объекта класса РСА по матрице Matrix

```
PCA pca(a)
```

* Применить к матрице метод главных компонент

```
auto e = pca.ResMatrix(n)
```

* Получить результирующую матрицу в виде двумерного массива

```
auto res = e.GetMatrix()
```

* Посчитать матрицу остатков

```
pca.ERVP(n)
```