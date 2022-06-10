import os

DLL_PATH = '/home/michael/iwfm-docker/build/iwfm'
DLL = 'libIWFMLib.so'

from pywfm.model import IWFMModel
from pywfm.budget import IWFMBudget
from pywfm.zbudget import IWFMZBudget
from pywfm.decorators import program_timer
