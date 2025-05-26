# Re-import and regenerate due to code state reset

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC

# Set seed
np.random.seed(42)

# Generate two normal distributions
n_samples = 100
mean_class0 = [4, 4]
mean_class1 = [5, 5]
cov = [[1, 0], [0, 1]]

class0 = np.random.multivariate_normal(mean_class0, cov, n_samples)
labels0 = np.zeros(n_samples)

class1 = np.random.multivariate_normal(mean_class1, cov, n_samples)
labels1 = np.ones(n_samples)

X = np.vstack((class0, class1))
y = np.concatenate((labels0, labels1))

# Split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, stratify=y, random_state=42)

# Train classifiers
nb = GaussianNB()
svm = SVC(probability=True, random_state=42)
nb.fit(X_train, y_train)
svm.fit(X_train, y_train)

# Predict
nb_preds = nb.predict(X_test)
svm_preds = svm.predict(X_test)

# Prepare DataFrame
df = pd.DataFrame({
    "x": X_test[:, 0],
    "y": X_test[:, 1],
    "true_label": y_test.astype(int),
    "nb_prediction": nb_preds.astype(int),
    "svm_prediction": svm_preds.astype(int)
})

# Plot distribution of true classes
colors = ['red' if label == 0 else 'blue' for label in df['true_label']]

plt.figure(figsize=(8, 6))
plt.scatter(df['x'], df['y'], c=colors, alpha=0.7, edgecolor='k', s=60)
plt.title("Rozkład punktów wg klas rzeczywistych")
plt.xlabel("x")
plt.ylabel("y")
plt.grid(True)
plt.legend(handles=[
    plt.Line2D([0], [0], marker='o', color='w', label='Klasa 0', markerfacecolor='red', markersize=10),
    plt.Line2D([0], [0], marker='o', color='w', label='Klasa 1', markerfacecolor='blue', markersize=10)
])
plt.tight_layout()
plt.show()

data_path_named = "./classification_test_data_named.csv"
df.to_csv(data_path_named, index=False)