

class Singleton(type):

    def __init__(cls, name,  bases, dict):
        super(Singleton, cls).__init__(name, bases, dict)
        cls.instance =  None


    def __call__(cls, *args, **kws):
        if not cls.instance:
            cls.instance = super(Singleton, cls).__call__(*args, **kws)
            cls.instance.SetupWindow()
        return cls.instance