# Re-execute due to code state reset
import pandas as pd
import numpy as np
from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC

# Load the Iris dataset
iris = load_iris()
X = iris.data
y = iris.target
target_names = iris.target_names

# Map numeric labels to class names
y_named = target_names[y]

# Split into train and test sets using named labels
X_train, X_test, y_train_named, y_test_named = train_test_split(
    X, y_named, test_size=0.3, random_state=42, stratify=y_named
)

# Train Random Forest and SVM models using numeric labels for training
# We still need numeric labels for fitting
y_train = [np.where(target_names == label)[0][0] for label in y_train_named]
y_test = [np.where(target_names == label)[0][0] for label in y_test_named]

rf = RandomForestClassifier(random_state=42)
svm = SVC(probability=True, random_state=42)

rf.fit(X_train, y_train)
svm.fit(X_train, y_train)

# Predict numeric class labels
rf_preds_numeric = rf.predict(X_test)
svm_preds_numeric = svm.predict(X_test)

# Convert numeric predictions back to names
rf_preds_named = target_names[rf_preds_numeric]
svm_preds_named = target_names[svm_preds_numeric]

# Create a DataFrame with named labels and predictions
data_named = pd.DataFrame({
    "true_label": y_test_named,
    "rf_prediction": rf_preds_named,
    "svm_prediction": svm_preds_named
})

# Save to CSV
data_path_named = "./classification_test_data_named.csv"
data_named.to_csv(data_path_named, index=False)

data_named.head()
