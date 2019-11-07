import click
import os
from download import download_from_ireceptor


@click.command()
@click.option('--url', help='url to download data')
def interface(url:str):
    if not os.path.isdir("./download"):
        os.mkdir("./download")
    if not os.path.isfile("./download/ireceptor_sequence_data.json"):
        data_file = download_from_ireceptor(url,"./download/ireceptor_sequence_data.json")





if __name__ == "__main__":
    interface()