# -*- coding: utf-8 -*-
"""
Created on Mon Dec  4 15:06:43 2023

@author: asweet0862
"""

import geopandas as gpd
import pandas as pd
from shapely.geometry import Point

# Reading a CSV formatted text file
df = pd.read_csv('latlondata.csv', sep=',')  # Change 'sep' based on your delimiter

# Load shapefile
zip_shapefile = gpd.read_file('path_to_zipcode_shapefile.shp')

# Create Point objects from lat-long
points = [Point(lon, lat) for lon, lat in zip(longitudes, latitudes)]

# Match points to polygons
for point in points:
    for zip in zip_shapefile.iterrows():
        if point.within(zip[1]['geometry']):
            print(f"Point {point} is in zip code {zip[1]['ZIP_CODE']}")
            break