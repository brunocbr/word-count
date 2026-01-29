from setuptools import setup

APP = ['ble_send_words_today.py']

OPTIONS = {
    'argv_emulation': False,
    'packages': ['bleak'],
    'plist': {
        'CFBundleName': 'BLE Word Count',
        'CFBundleDisplayName': 'BLE Word Count',
        'CFBundleIdentifier': 'org.wordcount.ble',
        'CFBundleVersion': '1.0',
        'CFBundleShortVersionString': '1.0',

        'NSBluetoothAlwaysUsageDescription':
            'This application sends daily word counts to a BLE device.',

        'LSUIElement': True,
    },
}

setup(
    app=APP,
    options={'py2app': OPTIONS},
    setup_requires=['py2app'],
)
