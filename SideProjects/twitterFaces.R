library(httr)

faceURL = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age,gender,smile,facialHair"
img.url = 'http://www.buro247.com/images/Angelina-Jolie-2.jpg'

faceKEY = 'your_ms_face_key'

mybody = list(url = img.url)

faceResponse = POST(
  url = faceURL, 
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = faceKEY)),
  body = mybody,
  encode = 'json'
)
faceResponse

AngelinaFace = content(faceResponse)[[1]]
names(AngelinaFace)

AngelinaFace$faceAttributes$gender

## Scrape the image URLs of the actresses
library(rvest)
library(XML)

linksactresses = 'http://www.imdb.com/list/ls050128191/'

out = read_html(linksactresses)
images = html_nodes(out, '.zero-z-index')
imglinks = html_nodes(out, xpath = "//img[@class='zero-z-index']/@src") %>% html_text()

## additional information, the name of the actress
imgalts = html_nodes(out, xpath = "//img[@class='zero-z-index']/@alt") %>% html_text()

### create an id and name for the face list
URL.face = "https://api.projectoxford.ai/face/v1.0/facelists/listofsexyactresses"

mybody = list(name = 'top 100 of sexy actresses')

faceLIST = PUT(
  url = URL.face,
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = faceKEY)),
  body = mybody,
  encode = 'json'
)
faceLIST

i=1
userdata = imgalts[i]
linkie = imglinks[i]
face.uri = paste(
  'https://api.projectoxford.ai/face/v1.0/facelists/listofsexyactresses/persistedFaces?userData=',
  userdata,
  sep = ";"
)
face.uri = URLencode(face.uri)
mybody = list(url = linkie )

faceLISTadd = POST(
  url = face.uri,
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = faceKEY)),
  body = mybody,
  encode = 'json'
)
faceLISTadd
print(content(faceLISTadd))

faceDetectURL = 'https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age,gender,smile,facialHair'
img.url = 'http://a.dilcdn.com/bl/wp-content/uploads/sites/8/2009/06/angelinaangry002.jpg'

mybody = list(url = img.url)

faceRESO = POST(
  url = faceDetectURL,
  content_type('application/json'), add_headers(.headers =  c('Ocp-Apim-Subscription-Key' = faceKEY)),
  body = mybody,
  encode = 'json'
)
faceRESO
fID = content(faceRESO)[[1]]$faceId

sim.URI = 'https://api.projectoxford.ai/face/v1.0/findsimilars'

mybody = list(faceID = fID, faceListID = 'listofsexyactresses')

faceSIM = POST(
  url = sim.URI,
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = faceKEY)),
  body = mybody,
  encode = 'json'
)
faceSIM
yy = content(faceSIM)
yy





