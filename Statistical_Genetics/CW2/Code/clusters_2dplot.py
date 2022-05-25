
##!!!## code is adapted from:  https://www.kaggle.com/minc33/visualizing-high-dimensional-clusters 


#Instructions for building the 2-D plot
import plotly as py
import plotly.graph_objs as go
from plotly.offline import download_plotlyjs, init_notebook_mode, plot, iplot
import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from sklearn.cluster import AgglomerativeClustering
from sklearn.metrics import silhouette_samples, silhouette_score
from sklearn.neighbors import NearestCentroid
import matplotlib.cm as cm
import matplotlib.pyplot as plt




class plot2dclust():
    def __init__(self, X,tp, clusters = None):
        self.clusters = clusters
        self.tp = tp
        self.X = X

    def make_plot(self, ker):
        X = self.X
        clusters = self.clusters
        nameofcol = 'cluster_' + self.tp
        X[nameofcol] = clusters
        #Try PCA for visulization
        plotX = X
        plotX.columns = X.columns

        #PCA with two principal components
        pca_2d = PCA(n_components=2)

        #This DataFrame contains the two principal components that will be used
        #for the 2-D visualization mentioned above
        PCs_2d = pd.DataFrame(pca_2d.fit_transform(plotX.drop([nameofcol], axis=1)))

        PCs_2d.columns = ["PC1_2d", "PC2_2d"]

        plotX = pd.concat([plotX,PCs_2d],axis=1, join='inner')
        cluster0 = plotX[plotX[nameofcol] == 0]
        cluster1 = plotX[plotX[nameofcol] == 1]
        #trace1 is for 'Cluster 0'
        trace1 = go.Scatter(
                            x = cluster0["PC1_2d"],
                            y = cluster0["PC2_2d"],
                            mode = "markers",
                            name = "Cluster 0",
                            marker = dict(color = 'rgba(255, 128, 255, 0.8)'),
                            text = None)

        #trace2 is for 'Cluster 1'
        trace2 = go.Scatter(
                            x = cluster1["PC1_2d"],
                            y = cluster1["PC2_2d"],
                            mode = "markers",
                            name = "Cluster 1",
                            marker = dict(color = 'rgba(255, 128, 2, 0.8)'),
                            text = None)

        data = [trace1, trace2]

        title = ""

        layout = dict(title = title,
                    xaxis= dict(title= 'PC1',ticklen= 5,zeroline= False),
                    yaxis= dict(title= 'PC2',ticklen= 5,zeroline= False)
                    )

        fig = dict(data = data, layout = layout)

        iplot(fig)
        # fig.write_image("plots/kmeans.pdf")

    def clustering(self, range_n_clusters = [2,3,5,8,12,15], aggclust = False):
        X = self.X
        siluets = []
        for n_clusters in range_n_clusters:
            # Create a subplot with 1 row and 2 columns
            if aggclust:
                # X = X.T
                clusterer = AgglomerativeClustering(n_clusters=n_clusters, linkage='ward')
                y_predict = clusterer.fit_predict(X)
                cluster_labels = clusterer.labels_
            else:
                y_predict = self.clusters
                cluster_labels = self.clusters

            clf = NearestCentroid()
            clf.fit(X, y_predict)

            silhouette_avg = silhouette_score(X, cluster_labels)
            siluets.append(silhouette_avg)
            # print("For n_clusters =", n_clusters,
            #         "The average silhouette_score is :", silhouette_avg)

            # if (n_clusters == 2 or n_clusters == 3 or n_clusters == 5):
            if (n_clusters == 2):

                # print("For n_clusters =", n_clusters,
                #     "The average silhouette_score is :", silhouette_avg)
                fig, (ax1) = plt.subplots(1, 1)

                fig.set_size_inches(15, 5)

                ax1.set_xlim([-0.1, 1])
                ax1.set_ylim([0, len(X) + (n_clusters + 1) * 10])

                sample_silhouette_values = silhouette_samples(X, cluster_labels)

                y_lower = 10
                for i in range(n_clusters):
                    ith_cluster_silhouette_values = \
                        sample_silhouette_values[cluster_labels == i]

                    ith_cluster_silhouette_values.sort()

                    size_cluster_i = ith_cluster_silhouette_values.shape[0]
                    y_upper = y_lower + size_cluster_i

                    color = cm.nipy_spectral(float(i) / n_clusters)
                    ax1.fill_betweenx(np.arange(y_lower, y_upper),
                                    0, ith_cluster_silhouette_values,
                                    facecolor=color, edgecolor=color, alpha=0.7)
                    ax1.text(-0.05, y_lower + 0.5 * size_cluster_i, str(i))
                    y_lower = y_upper + 10  # 10 for the 0 samples

                ax1.set_title("The silhouette plot for the various clusters.")
                ax1.set_xlabel("The silhouette coefficient values")
                ax1.set_ylabel("Cluster label")
                ax1.axvline(x=silhouette_avg, color="red", linestyle="--")

        plt.savefig('plots/single_siluette.pdf')
        plt.show()
        return(siluets)