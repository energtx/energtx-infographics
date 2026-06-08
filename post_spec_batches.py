#!/usr/bin/env python3
"""
post_to_bluesky.py — upload the generated charts + captions to
@energtx.bsky.social using the atproto lib.

Usage:
    pip install atproto python-dotenv
    cp .env.example .env           # fill in your App Password
    python3 scripts/post_to_bluesky.py               # dry-run (first 3)
    python3 scripts/post_to_bluesky.py --send        # actually post
    python3 scripts/post_to_bluesky.py --send --from 10 --to 30
    python3 scripts/post_to_bluesky.py --send --id 042

Hashtag facets are attached automatically so each #tag becomes a clickable link.
Rate-limit: sleeps between posts to stay well under Bluesky's quota.
"""
from __future__ import annotations
import argparse, json, os, re, sys, time, pathlib
from typing import List

ROOT = pathlib.Path(__file__).resolve().parents[1]
IMG  = ROOT / "images"
POSTS = ROOT / "posts"

def load_env():
    env = {}
    fn = ROOT / ".env"
    if fn.exists():
        for line in fn.read_text().splitlines():
            line = line.strip()
            if not line or line.startswith("#") or "=" not in line:
                continue
            k, v = line.split("=", 1)
            env[k.strip()] = v.strip()
    for k in ("BLUESKY_HANDLE","BLUESKY_APP_PASSWORD","BLUESKY_SERVICE"):
        env.setdefault(k, os.environ.get(k,""))
    return env

def build_facets(text: str):
    """Return atproto Facet objects for each #hashtag and URL found in text."""
    try:
        from atproto_client import models
    except Exception:
        from atproto import models  # fallback
    facets = []
    utf8 = text.encode("utf-8")
    # hashtags
    for m in re.finditer(r"#(\w+)", text):
        byte_start = len(text[:m.start()].encode("utf-8"))
        byte_end   = len(text[:m.end()].encode("utf-8"))
        facets.append(models.AppBskyRichtextFacet.Main(
            index=models.AppBskyRichtextFacet.ByteSlice(
                byte_start=byte_start, byte_end=byte_end),
            features=[models.AppBskyRichtextFacet.Tag(tag=m.group(1))],
        ))
    # URLs
    for m in re.finditer(r"https?://[^\s]+", text):
        byte_start = len(text[:m.start()].encode("utf-8"))
        byte_end   = len(text[:m.end()].encode("utf-8"))
        facets.append(models.AppBskyRichtextFacet.Main(
            index=models.AppBskyRichtextFacet.ByteSlice(
                byte_start=byte_start, byte_end=byte_end),
            features=[models.AppBskyRichtextFacet.Link(uri=m.group(0))],
        ))
    return facets

def list_ids() -> List[str]:
    return sorted(p.stem for p in POSTS.glob("*.txt"))

def send_one(client, sid: str):
    from atproto import models
    txt_path = POSTS / f"{sid}.txt"
    img_path = IMG / f"{sid}.webp"
    text = txt_path.read_text().strip()
    if len(text) > 300:
        print(f"[{sid}] text too long ({len(text)} chars), truncating")
        text = text[:297] + "..."
    with open(img_path, "rb") as f:
        blob = f.read()
    alt = text.splitlines()[0][:800]
    client.send_image(text=text, image=blob, image_alt=alt,
                      facets=build_facets(text))

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--send", action="store_true")
    ap.add_argument("--id", default=None)
    ap.add_argument("--from", dest="start", type=int, default=1)
    ap.add_argument("--to",   dest="end",   type=int, default=3)
    ap.add_argument("--sleep", type=float, default=12.0)
    args = ap.parse_args()

    env = load_env()
    if args.send:
        if not env["BLUESKY_APP_PASSWORD"] or env["BLUESKY_APP_PASSWORD"].startswith("xxxx"):
            sys.exit("ERROR: fill BLUESKY_APP_PASSWORD in .env first")
        from atproto import Client
        client = Client(base_url=env.get("BLUESKY_SERVICE","https://bsky.social"))
        client.login(env["BLUESKY_HANDLE"], env["BLUESKY_APP_PASSWORD"])
        print(f"logged in as {env['BLUESKY_HANDLE']}")

    ids = list_ids()
    if args.id:
        targets = [args.id]
    else:
        targets = ids[args.start-1:args.end]

    for sid in targets:
        print(f"[{sid}] {(POSTS/f'{sid}.txt').read_text().splitlines()[0]}")
        if args.send:
            try:
                send_one(client, sid)
                print(f"  posted.")
            except Exception as e:
                print(f"  err: {e}")
            time.sleep(args.sleep)

    if not args.send:
        print("\n(dry-run) pass --send to publish")

if __name__ == "__main__":
    main()
