import os
from distutils.core import setup, Extension

buoy_module = Extension('buoy._buoy',
                        sources=['buoymodule.c', '../buoy.c', '../lexi.c'],
                        include_dirs=['..'],
                        extra_compile_args=['-std=c99'])

setup(name='buoy',
      version='0.1',
      description='A fast, simple learning library.',
      author='Jared Flatow',
      ext_modules=[buoy_module],
      packages=['buoy'])
