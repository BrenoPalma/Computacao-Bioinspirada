from sklearn import datasets
from sklearn.model_selection import train_test_split
from sklearn.linear_model import Perceptron
from sklearn.metrics import accuracy_score
import numpy as np
# Carregar o conjunto de dados Iris
iris = datasets.load_iris()
X = iris.data
y = iris.target

X = X[y != 2]
y = y[y != 2]

# Dividir o conjunto de dados em treinamento e teste (70% treinamento, 30% teste)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Criar e treinar o Perceptron com taxa de aprendizado ajustada
learning_rate = 0.000001  # Altere a taxa de aprendizado aqui
perceptron = Perceptron(eta0=learning_rate, max_iter=10, random_state=42)
perceptron.fit(X_train, y_train)

print("Porcentagem treinada: 0.3")
print("Taxa de Aprendizado: 0.000001")
print("Número de iterações: 10")
# Fazer previsões no conjunto de teste
y_pred = perceptron.predict(X_test)

# Calcular a precisão do modelo
accuracy = accuracy_score(y_test, y_pred)
print(f'Acurácia do Perceptron: {accuracy * 100:.2f}%')