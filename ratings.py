import openai
import pandas as pd
import numpy as np
import pyreadr

with open("secret", "r") as f: 
  openai.api_key = f.read()
  
development = pyreadr.read_r("data/development.rds")
development = development[None]
development = development.sample(5)
essays = development["response"].tolist()
ids = development["id"].tolist()

responses = np.empty((len(essays)), dtype=object)

for i,essay in enumerate(essays):
  print(essay)
  print(i)
  print(ids[i])
  response = openai.ChatCompletion.create(
      model="gpt-3.5-turbo",
      messages=[
              {"role": "system", "content": "You are an admissions officer that reads students essays about personal qualities and gives them a score between 0 and 100 for seven personal qualities. Answer with a score and an explanation for each. The qualities are: Prosocial Purpose: Helping others, wanting to help others, consideration of the benefits to others, mention of reasons for helping others, or reflection on how enjoyable or rewarding it is to help others. Leadership: Serving in a leadership role, commenting on what he or she did in his or her capacity as a leader, or discuss the value, meaning, or importance of leadership. Learning: Improving, learning, or developing knowledge, skills, or abilities. Goal pursuit: Having a goal and/or a plan. Intrinsic motivation: Describing the activity as enjoyable or inter- esting. Liking the activity or identifying with it. Teamwork: Working with or learning from others. Valuing what fellow participants bring to the activity. Perseverance: Persisting in the face of challenge especially for extended periods of time. Answer with the personal quality, folowed by a dash, the score, a dash, and an explanation. Put each personal quality on a new line."},
              {"role": "user", "content": essay},
          ]
  )
  result = response.choices[0].message.content
  responses[i] = result
  with open("raw_results.txt", "a") as f:
    f.write(result)
    f.write("\n")
  



# OLD CODE MAYBE USEFUL
result = response.choices[0].message.content
list = result.split("\n\n")
df = pd.DataFrame(list, columns=["text"])
df[['text', "score"]] = df.text.str.split(":", expand=True)
df[['score', "explanation"]] = df.score.str.split(".",1, expand=True)
