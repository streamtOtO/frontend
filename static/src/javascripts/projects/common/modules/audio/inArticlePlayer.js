import { sendToOphan } from './utils';

const init = () => {
    const inArticlePlayer = document.querySelector('.in-article-audio');
    const mediaId = inArticlePlayer.getAttribute('data-media-id');

    monitorPlay(inArticlePlayer, mediaId);

    monitorPercentPlayed(inArticlePlayer, 25, mediaId);
    monitorPercentPlayed(inArticlePlayer, 50, mediaId);
    monitorPercentPlayed(inArticlePlayer, 75, mediaId);
    monitorPercentPlayed(inArticlePlayer, 99, mediaId);
}


const monitorPlay = (inArticlePlayer, id) => {
    inArticlePlayer.addEventListener('play', () => sendToOphan(id, 'play'));
}

const monitorPercentPlayed = (inArticlePlayer, marker, id) => {

    inArticlePlayer.addEventListener(
        'timeupdate',
        function listener(e){
            let percentPlayed = Math.round(
                (e.target.currentTime / e.target.duration) * 100
            );
            let eventName = marker === 99 ? 'end' : marker.toLocaleString();
            if(percentPlayed > marker) {
                sendToOphan(id, eventName);
                e.target.removeEventListener(e.type, listener)
            }
        }
    );
};

export {init};
