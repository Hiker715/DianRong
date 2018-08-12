import pandas as pd
from sklearn import preprocessing
import requests
import json
import re
import time 




anjuke_sample = pd.read_csv('/Users/heisenberg/Downloads/研究生课件/13项目数据/1安居客租房数据/上海租房.csv')

for i in range(len(anjuke_sample)):
    try:
        anjuke_sample.loc[i, '租金'] = float(str(anjuke_sample.loc[i, '租金']).replace('元/月',''))
        anjuke_sample.loc[i, '面积'] = float(str(anjuke_sample.loc[i, '面积']).replace('平米', ''))
        anjuke_sample.loc[i, '物业费'] = float(str(anjuke_sample.loc[i, '物业费']).replace('元/平米/月', ''))
        anjuke_sample.loc[i, '建造年代'] = float(str(anjuke_sample.loc[i, '建造年代']).replace('年', ''))
    except Exception:
        pass

anjuke_sample.rename(columns = {'租金':'租金(元/月)', '面积':'面积(平方米)', '物业费':'物业费(元/平方米/月)',
                                '建造年代':'建造年代(年)'}, inplace = True)

import re

for i in range(len(anjuke_sample)):
    try:
        anjuke_sample.loc[i, '交通'] = int(re.findall('[:,1][0-9]{2}%', anjuke_sample.loc[i, '交通'])[0].replace(':','').replace('%',''))
        anjuke_sample.loc[i, '医疗'] = int(re.findall('[:,1][0-9]{2}%', anjuke_sample.loc[i, '医疗'])[0].replace(':','').replace('%',''))
        anjuke_sample.loc[i, '教育'] = int(re.findall('[:,1][0-9]{2}%', anjuke_sample.loc[i, '教育'])[0].replace(':','').replace('%',''))
        anjuke_sample.loc[i, '商业'] = int(re.findall('[:,1][0-9]{2}%', anjuke_sample.loc[i, '商业'])[0].replace(':','').replace('%',''))
    except Exception:
        pass


for i in range(len(anjuke_sample)):
    try:
        anjuke_sample.loc[i, '租金押付'] = anjuke_sample.loc[i, '租金押付'].replace('我要贷款', '')
        anjuke_sample.loc[i, '地址'] = anjuke_sample.loc[i, '地址'].replace('(地图)', '')
        anjuke_sample.loc[i, '物业费(元/平方米/月)'] = float(anjuke_sample.loc[i, '物业费(元/平方米/月)'].replace('暂无', ''))

    except Exception:
        pass

def isNum(value):
    try:
        value + 1
    except TypeError:
        return False
    else:
        return True

anjuke_sample = anjuke_sample.iloc[[len(str(anjuke_sample.iloc[i,15]))<4 for i in range(len(anjuke_sample))],:]
anjuke_sample = anjuke_sample.iloc[[len(str(anjuke_sample.iloc[i,16]))<4 for i in range(len(anjuke_sample))],:]
anjuke_sample = anjuke_sample.iloc[[len(str(anjuke_sample.iloc[i,17]))<4 for i in range(len(anjuke_sample))],:]

for i in range(len(anjuke_sample)):
    try:
        int(float(anjuke_sample.loc[i, '建造年代(年)']))
    except Exception:
        anjuke_sample.drop([i], inplace=True)


columns = [4, 5, 6, 8, 9, 11, 13]
mapList = []

for col in columns:
    le = preprocessing.LabelEncoder()
    le.fit(list(set(anjuke_sample.iloc[:, col])))
    anjuke_sample.iloc[:, col] = le.transform(anjuke_sample.iloc[:, col])
    mapList.append(le.classes_)

for i in range(len(anjuke_sample)):
    if not isNum(anjuke_sample.loc[i, '楼层']):
        if anjuke_sample.loc[i, '楼层'].find('/') != -1:
            num = anjuke_sample.loc[i, '楼层'].split('/')
            percent = float(num[0])/float(num[1])
            if percent < 1/3:
                anjuke_sample.loc[i, '楼层'] = 0
            elif percent < 2/3:
                anjuke_sample.loc[i, '楼层'] = 1
            else:
                anjuke_sample.loc[i, '楼层'] = 2
        else:
            anjuke_sample.drop([i], inplace=True)
    else:
        continue



def geocodeB(address):
    base = "http://api.map.baidu.com/geocoder/v2/?address=" + address + "&output=json&ak=SapBBjbxXQEyV21P6iji2pBO7QYKKTd8&callback=showLocation"
    response = requests.get(base)
    temp = re.sub('showLocation&&showLocation\(','', response.content.decode()).replace(')', '')
    lng = json.loads(temp)['result']['location']['lng']
    lat = json.loads(temp)['result']['location']['lat']
    return lng, lat

def getLngLat(residual, district_code=None, district_dict=None):
    district = ''
    if district_code != None and district_dict != None:
        district = district_dict[district_code]
    address = '上海市'+district+residual
    lng, lat = geocodeB(address)
    return lng, lat


f = open('/Users/heisenberg/Downloads/研究生课件/1统计软件/subway.csv', encoding='gbk')
sub_list = []
for line in f.readlines():
    sub_list.append(line.split('\n')[0])
sub_list = sub_list[1:]
sub_list = ['上海市' + re.sub(r'(\"|\"| )', '', item) + '地铁站' for item in sub_list]


sub_lnglat_dict = {}
for item in sub_list:
    sub_lnglat_dict[item] = [getLngLat(item)]
    time.sleep(0.1)