def format_bands(bands):
    for band in bands:
        band['country'] = 'Canada'
        band['name'] = band['name'].replace('.', '')
        band['name'] = band['name'].title()


def assoc(_d, key, value):
    from copy import deepcopy
    d = deepcopy(_d)
    d[key] = value
    return d


def pipeline_each(data, fns):
    import functools
    return functools.reduce(lambda a, fn: list(map(fn, a)), fns, data)


def set_canada_as_country(bands):
    return assoc(bands, 'country', 'Canada')


def strip_punctuation_from_name(bands):
    key = 'name'
    valueToModify = bands[key]
    return assoc(bands, key, valueToModify.replace('.', ''))


def capitalize_names(bands):
    key = 'name'
    valueToModify = bands[key]
    return assoc(bands, key, valueToModify.title())


if __name__ == '__main__':
    bands = [{'name': 'sunset rubdown', 'country': 'UK', 'active': False},
             {'name': 'women', 'country': 'Germany', 'active': False},
             {'name': 'a silver mt. zion', 'country': 'Spain', 'active': True}]

    print(pipeline_each(bands, [set_canada_as_country,
                                strip_punctuation_from_name,
                                capitalize_names]))
