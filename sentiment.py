
# Make sure to use version 1.3.0+ of stanza, otherwise you will get an md5 error here.
import stanza
import numpy as np

# This downloads the English models for the neural pipeline
#stanza.download('en')
#stanza.download('en', package='mimic')

# Use the MIMIC-customized tokenizer but the
# default sentiment processor (MIMIC does not provide one).
processor_dict = {
    'tokenize': 'mimic', 
    'sentiment': 'default'
}

nlp_stanza = stanza.Pipeline(lang = 'en',
                             # Mimic package doesn't have sentiment unfortunately.
                             #package = 'mimic',
                             #processors = 'tokenize,sentiment',
                             processors = processor_dict,
                             package = None,
                             # To allow parallelization by CPU.
                             use_gpu = False,
                             logging_level = 'WARN',
                             verbose = False)

def sentiment_stanza(text):
    doc = nlp_stanza(text)
    sentiments = []
    # This is intended to be only a single sentence, however
    # stanza's sentence segmentation has the potential to subdivide
    # it into more than 1 sentence.
    for i, sentence in enumerate(doc.sentences):
        sentiments.append(sentence.sentiment)
        
    # TODO: weight by sentence length.
    return np.mean(sentiments)

# Source: deep-learning-model.ipynb
# Text should be a single string, not a vector currently.
def sentiment_hf(text, model, tokenizer, max_length = 512):
    # prepare our text into tokenized sequence
    inputs = tokenizer(text, padding=True, truncation=True, max_length=max_length, return_tensors="pt").to("cuda")
    # perform inference to our model
    outputs = model(**inputs)
    # get output probabilities by doing softmax
    probs = outputs[0].softmax(1)
    # executing argmax function to get the candidate label
    return class_names[probs.argmax()]