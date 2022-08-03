# add date as a band to image collection
def addDate(image):
    img_date = ee.Date(image.date())
    img_date = ee.Number.parse(img_date.format('YYYYMMdd'))
    return image.addBands(ee.Image(img_date).rename('date').toInt())
    
# find hottest period
def get_hottest_period(Districts,start_date,end_date,ROIcenter):
    # select dataset, filter by dates and visualize
    dataset = (ee.ImageCollection('NASA/NEX-GDDP')
               .filter(ee.Filter.And(
                   ee.Filter.date(start_date, end_date),
                   ee.Filter.eq('scenario','rcp85'),
                    ee.Filter.eq('model','BNU-ESM'),
                   ee.Filter.bounds(Districts)
               )
                      )
              )
    AirTemperature = dataset.select(['tasmax'])
    
    withdates = AirTemperature.map(addDate)
    
    # create a composite with the hottest day value and dates for every location and add to map
    hottest = withdates.qualityMosaic('tasmax')
    
    # reduce composite to get the hottest date for centroid of ROI
    resolution = dataset.first().projection().nominalScale()
    NEXtempMax = ee.Number(hottest.reduceRegion(ee.Reducer.firstNonNull(), ROIcenter, resolution).get('date'))
    
    # convert date number to date type
    date = ee.Date.parse('YYYYMMdd',str(NEXtempMax.getInfo()))
    
    # calculate 45 days before and after hottest date.  Format as short date.
    start90days = date.advance(-44, 'day').format('YYYY-MM-dd')
    end90days = date.advance(45, 'day').format('YYYY-MM-dd')
    print(start90days.getInfo())
    print(end90days.getInfo())
    start90days_info = start90days.getInfo()
    end90days_info = end90days.getInfo()
    
    return(start90days_info,end90days_info)