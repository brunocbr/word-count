#!/usr/bin/env python3
import sys
import struct
import asyncio
from time import time
from bleak import BleakScanner, BleakClient

SERVICE_UUID = "0000fff0-0000-1000-8000-00805f9b34fb"
DATETIME_UUID = "0000fff3-0000-1000-8000-00805f9b34fb"
WORDS_TODAY_UUID = "0000fff7-0000-1000-8000-00805f9b34fb"

async def find_device():
    """Scan for a BLE device advertising our service UUID."""
    try:
        device = await BleakScanner.find_device_by_filter(
            lambda d, ad: SERVICE_UUID.lower() in [u.lower() for u in (ad.service_uuids or [])],
            timeout=5.0
        )
        return device
    except Exception as e:
        print(f"Error scanning for device: {e}", file=sys.stderr)
        return None

async def main():
    # Check argument
    if len(sys.argv) < 2:
        print("Usage: python ble_send_words_today.py <word_count>", file=sys.stderr)
        sys.exit(1)

    try:
        count = int(sys.argv[1])
    except ValueError:
        print(f"Invalid word count: {sys.argv[1]}", file=sys.stderr)
        sys.exit(1)

    # Find the device
    device = await find_device()
    if device is None:
        print("Device not found", file=sys.stderr)
        sys.exit(.1)

    # Connect and write characteristics
    try:
        async with BleakClient(device) as client:
            await client.write_gatt_char(WORDS_TODAY_UUID, struct.pack("<I", count), response=False)
            epoch = int(time()) & 0xFFFFFFFF
            await client.write_gatt_char(DATETIME_UUID, struct.pack("<I", epoch), response=False)
            await asyncio.sleep(1)
    except Exception as e:
        print(f"Error updating device: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    try:
        asyncio.run(main())
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        sys.exit(1)
