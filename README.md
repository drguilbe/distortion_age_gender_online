# The Invisibility of Older Women Online

This github contains all of the replication materials for Guilbeault, Delecourt, and Desikan 2024, "The Invisibility of Older Women Online"

This git contains: <br>
-R scripts for replicating all main and supplementary analyses from the raw data. The name of the R script and header text in the script specify which analyses it replicates from the manuscript and SI. <br>
-The raw data for all observational analyses and experiments<br>

All data for replicating the main and supplementary analyses is contained in the following Google Drive folder: https://drive.google.com/drive/folders/1-4cy2gYDDRZ8tnZ6ZXES3jcwxNiNucG3?usp=sharing 

#Here is a column guide to the main image and text datasets (nature_data_obsv.csv): 

Social.Category: The Social Category from Wordnet
WorkerId: The MturkID of the Mturker who classified the image. 
face_id: The unique identifier for the face extracted from the image. 	
image_id: The unique identifier for the image.	
Human.Face: Did the coder identify the cropped image as containing a human face? Yes or no. 
Img.Gender: Coder’s gender classification of face. 
Img.Age: Coder’s age classification of face. 
Attention.Check: Did the coder pass the attention check?	
avatar: Did the coder identify the cropped image as an avatar? Yes or no.	
searchDEMO: The specified demographic included with the Google search of the social category.
Data.Source: Source of image (Google or Wikipedia) 
Img.Age.Cat: Coder’s age classification of face (categorical variable)
coder_ideo: Self-identified political ideology of coder
coder_gender: Self-identified gender of coder
coder_race: Self-identified race of coder	
coder_age: Self-identified age of coder	
coder_inc: Self-identified income level of coder	
coder_edu: Self-identified highest education level of coder	
Gendered.Category: does the category explicitly imply gender? Yes or no 
GoogleNews.300D.Gender: embedding gender association (-1 female, 1 male) of social category in the Google News word2vec model (300 dimensions) 
Google.Img.US.Search.Freq: national-level search frequency of social category in Google Image search
Human.Gender.Judge: Human ratings of gender of social category	
Wiki.50D.Gender: embedding gender association (-1 female, 1 male) of social category in the Wikipedia Glove model (50 dimensions)
Wiki.100D.Gender: embedding gender association (-1 female, 1 male) of social category in the Wikipedia Glove model (100 dimensions)	
Wiki.200D.Gender: embedding gender association (-1 female, 1 male) of social category in the Wikipedia Glove model (200 dimensions)	
Wiki.300D.Gender: embedding gender association (-1 female, 1 male) of social category in the Wikipedia Glove model (300 dimensions)	
Twitter.200D.Gender: embedding gender association (-1 female, 1 male) of social category in the Twitter Glove model (200 dimensions)	
cps_majorgroup: census grouping of social category (major) 	
cps_group: census grouping of social category	
cps_occupation: census occupational label of social category	
census.women: fraction of women in occupation (2020 census) 
Polysemy: number of definitions associated with social category in WordNet
Word.Frequency.General: frequency of word in U.S. population
Word.Frequency.Scaled: frequency of word in U.S. population (logged) 	
GoogleNews.300D.Gender.Norm: min-max normalization of “GoogleNews.300D.Gender” 	
Bert: embedding gender association (-1 female, 1 male) of social category in Bert
Bert.Norm: min-max normalization of “Bert” 		
ConceptNet: embedding gender association (-1 female, 1 male) of social category in ConceptNet	
ConceptNet.Norm: min-max normalization of “ConceptNet”
FastText: embedding gender association (-1 female, 1 male) of social category in FastText model
FastText.Norm: min-max normalization of “FastText”	
Glove.Twitter: embedding gender association (-1 female, 1 male) of social category in the Twitter Glove model (standard) 
Glove.Twitter.Norm: min-max normalization of “Glove.Twitter”	
Glove.Wiki: embedding gender association (-1 female, 1 male) of social category in the Wikipedia Glove model (standard) 
Glove.Wiki.Norm: min-max normalization of “Glove.Wiki”		
GoogleNews.300D.Gender.v2: version 2 of gender poles for extracting “GoogleNews.300D.Gender”
GoogleNews.300D.Gender.v3: version 3 of gender poles for extracting “GoogleNews.300D.Gender”	
GoogleNews.300D.Gender.v4: version 4 of gender poles for extracting “GoogleNews.300D.Gender”	
GoogleNews.300D.Gender.v2.Norm: min-max normalization of “GoogleNews.300D.Gender.v2”	
GoogleNews.300D.Gender.v3.Norm: min-max normalization of “GoogleNews.300D.Gender.v3”	
GoogleNews.300D.Gender.v4.Norm: min-max normalization of “GoogleNews.300D.Gender.v4”	
Wiki.300D.Gender.Norm: min-max normalization of “Wiki.300D.Gender”	
Wiki.50D.Gender.Norm: min-max normalization of “Wiki.50D.Gender”		
Wiki.100D.Gender.Norm: min-max normalization of “Wiki.100D.Gender”		
Wiki.200D.Gender.Norm: min-max normalization of “Wiki.200D.Gender”		
GPT3.gender: embedding gender association (-1 female, 1 male) of social category in the GPT3
GPT3.gender.norm: min-max normalization of “GPT3.gender”
retrained.word2vec.300: embedding gender association (-1 female, 1 male) of social category in word2vec model trained on recent online news (2020 to present) 
retrained.word2vec.300.Norm: min-max normalization of “retrained.word2vec.300”
age_implied: is age explicitly connoted by social category 
