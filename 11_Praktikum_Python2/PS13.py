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


def call(fn, key):
    def apply_fn(record):
        return assoc(record, key, fn(record.get(key)))
    return apply_fn


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

    print(f"\nThis is bands, which should be unchanged because \
    pipeline_each method acts on deep copies of bands due to functional approach: \n{bands}")

    # Approach with call() method:
    set_canada_as_country = call(lambda x: 'Canada', 'country')
    strip_punctuation_from_name = call(lambda x: x.replace('.', ''), 'name')
    capitalize_names = call(lambda x: x.title(), 'name')

    print(f"\nThis is bands done with the call method approach:")
    print(pipeline_each(bands, [set_canada_as_country,
                                strip_punctuation_from_name,
                                capitalize_names]))
