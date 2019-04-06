#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, re
reload(sys)
sys.setdefaultencoding('utf-8')

path = "rakuten-eval-utf8.txt"

# テキストをエリアごとに結合する
text1 = ""
text2 = ""
text3 = ""
for i, line in enumerate(open(path, 'r')):
    line = unicode(line).strip()
    area = line.split('\t')[0]
    if area == u'01-外房': text1 += line.split('\t')[3]
    elif area == u'02-西伊豆': text2 += line.split('\t')[3]
    elif area == u'03-南房総': text3 += line.split('\t')[3]
    else: continue
texts = [text1, text2, text3]

# テキスト列を形態素解析する (エリアごと)
import MeCab
tagger = MeCab.Tagger()
data = []
for text in texts:
    encoded_text = text.encode('utf-8')
    node = tagger.parseToNode(encoded_text)
    terms = []
    while node:
        feature = re.split('[\s,]', (node.feature.decode('utf-8')).strip())
        if feature[0] == u'名詞' or feature[0] == u'形容詞' or feature[0] == u'動詞':
            terms.append(feature[6])
        node = node.next
    data.append(u'\t'.join(terms))

# 単語頻度ベクトルを作る
from sklearn.feature_extraction.text import CountVectorizer
def splitter(text): return text.split('\t')
vectorizer = CountVectorizer(analyzer=splitter, min_df=1, max_features=150)
features = vectorizer.fit_transform(data)

# 主成分分析
from sklearn.preprocessing import StandardScaler
Xs = StandardScaler().fit_transform(features.toarray())
from sklearn.decomposition import PCA
pca = PCA(n_components=2)
Xr = pca.fit_transform(Xs)
loadings = pca.components_.transpose()

# プロット
import matplotlib.pyplot as plt
plt.figure()
plt.title(u"主成分分析")

# 主成分得点のプロット
X, Y = Xr[:, 0], Xr[:, 1]
ax1 = plt.subplot(1,1,1)
ax1.scatter(X, Y, edgecolors="none", facecolors="none", label="none")
for x, y, l in zip(X, Y, [u"01-外房", u"02-西伊豆", u"03-南房総"]):
    ax1.text(x, y, l, ha='center', va="center", size=10, color="black")

# 主成分負荷量のプロット
l1, l2 = loadings[:,0], loadings[:,1]
ax2 = ax1.twiny().twinx()
ax2.set_xlim(min(l1)/0.75, max(l1)/0.75)
ax2.set_ylim(min(l2)/0.75, max(l2)/0.75)
ax2.set_xticks([])
ax2.scatter(l1, l2, edgecolors="none", facecolors="none", label="1")
for x, y, l in zip(l1, l2, vectorizer.get_feature_names()):
    ax2.text(x, y, l, ha='center', va="center", size=10, color="red")

plt.savefig('lecture-34-py1.png')
plt.show()
