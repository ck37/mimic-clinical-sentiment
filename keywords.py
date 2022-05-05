from collections import defaultdict
import re

class KeywordFinder():
    '''
    class to collect positive and negative sentiment keywords and return 
    sentiment score.
    '''
    def __init__(self, 
                 positive_keywords=[], 
                 negative_keywords=[]):
        '''
        init function. Cretes regex for positive and negative keywords and 
        map between positive and negative sentiment words.

        INPUTS:
            positive_keywords (list): list of positive sentiment keywords
            negative_keywords (list): list of negative sentiment keywords

        OUTPUTS:
            None

        DEMO USAGE:
            >>> positive_keywords = ['good', 'best']
            >>> negative_keywords = ['not good', 'worst']
            >>> kwf = KeywordFinder(positive_keywords=positive_keywords,
            ...                     negative_keywords=negative_keywords)
            >>>
            >>> text = 'it was a good day'
            >>> hits, score = kwf.run(text)
            >>> print(score)
            ...    0
            >>> print(hits)
            ...    {'good' : 1}
            >>>
            >>> text = 'I was not good today'
            >>> hits, score = kwf.run(text)
            >>> print(score)
            ...    1
            >>> print(hits)
            ...    {'not good' : 1}
            >>>
            >>> text = 'it was the best of times, it was the worst of times'
            >>> hits, score = kwf.run(text)
            >>> print(score)
            ...    0.5
            >>> print(hits)
            ...    {'best' : 1, 'worst' : 1}
            >>>
            >>> text = 'sentence void of sentiment'
            >>> hits, score = kwf.run(text)
            >>> print(score)
            ...    None
            >>> print(hits)
            ...    {}
        '''
        # clean input and generate mapping for keywords
        self.negative_keywords=[i.strip().lower() for i in negative_keywords]
        self.positive_keywords=[i.strip().lower() for i in positive_keywords]
        self.sent_map = {word : 'positive' for word in self.positive_keywords}
        self.sent_map.update({word : 'negative' 
                              for word in self.negative_keywords})

        # build regex. 
        # Ordered longest to shortest by keyword length to prevent miss counts
        self.keywords = self.positive_keywords + self.negative_keywords        
        kws = sorted(set(self.keywords), key=lambda x: -len(x))
        self.regex = re.compile(r'\b(' +  r')|('.join(kws) + r')\b')

        
    def find_keywords(self, text):
        '''
        function to find keywords.


        INPUTS:
            text (str): text to collect and score

        OUTPUTS:
            hits (dict): dictionary mapping keywords to total mentions in text
        '''
        # apply regex and count matches
        hits = defaultdict(int)
        for match in re.finditer(self.regex, text.lower()):
            hits[match.group()] += 1
            
        # convert from defaultdict to dict
        return dict(hits)
    
    
    def score_sentiment(self, hits):
        '''
        function to score keyword sentiment


        INPUTS:
            hits (dict): dictionary mapping keywords to total mentions in text

        OUTPUTS:
            score (float): sentiment score ranging from 0 to 1, 
                           with 1 being extremely negative
        '''
        # if no keywords return None
        if len(hits) == 0:
            return None
        
        # count total negative and positive keywords
        score = defaultdict(int)
        for kw, cnt in hits.items():
            score[self.sent_map[kw]] += cnt
            
        return score['negative'] / (score['negative'] + score['positive'])
    
    
    def run(self, text):
        '''
        function to run KeywordFinder.

        INPUTS:
            text (str): text to collect and score

        OUTPUTS:
            hits (dict): dictionary mapping keywords to total mentions in text
            score (float): sentiment score ranging from 0 to 1, 
                           with 1 being extremely negative
        '''
        hits = self.find_keywords(text)
        return hits, self.score_sentiment(hits)


# TODO: can we upgrade to spacy PhraseMatcher? - https://spacy.io/api/phrasematcher
def find_keywords2(text, keywords, verbose : bool = False):
    """
    Identifies any keywords/phrases in a text string, returning their location.
    
    Parameters
    ----------
    text : str
        A text sequence, e.g. a sentence or paragraph.
    
    keywords : list
        A list of keywords/phrases to be searched for. Will be lowercased.
        
    verbose : boolean
        If True, display additional output during execution.
    """
    
    found_keywords = {}
    text = str(text).lower()
    for keyword_i in keywords:
        # Extract all keywords that contain the current keyword.
        super_kws = [kw for kw in set(keywords).difference([keyword_i]) if keyword_i in kw]
        
        # Extract the found locations of any super keywords.
        found_super = {key: value for key, value in found_keywords.items() if key in super_kws}
 
        if verbose and len(found_super) > 0:
            print(f'Super keywords for {keyword_i}:', ", ".join(list(found_super.keys())))
        
        # Identify locations for this keyword.
        pattern = re.compile(r'\b{}\b'.format(keyword_i))
        r = pattern.search(text)
        
        # Loop over each span found for this keyword.
        found_locations = []
        while r:

            location = (r.start(), r.end() - 1)
            keep_location = True
            
            # Check that this location is not in any super keywords
            for skw, locations in found_super.items():
                # Each location is a (start, end) tuple.
                in_location = [location[0] >= loc[0] and location[1] <= loc[1] for loc in locations]
                # Check if our current keyword location was found in any existing spans.
                if sum(in_location) > 0:
                    # The target span has already been allocated to a longer keyword, so don't save
                    # this location.
                    keep_location = False
                    if verbose:
                        print(f'Found "{keyword_i}" in an earlier "{skw}" span - skipping this location.')
                    break
          
            # Add any remaining spans to the dictionary.
            if keep_location:
                found_locations.append(location)
            
            # Go to the next match, if any.
            r = pattern.search(text, r.start() + 1)
        
        # If any locations remain, add this keyword to the found list.
        if len(found_locations) > 0:
            found_keywords[keyword_i]  = found_locations
            
    # Note: the number of locations for each entry of found_keywords allows
    # one to count the number of times the keyword was found in the text.
    return found_keywords